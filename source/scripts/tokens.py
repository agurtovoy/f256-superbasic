from typing import Self, Sequence, TextIO
import re

from pathlib import Path


class Token(object):
    ID_RANGE: range
    RESERVED_PREFIX = "RESERVED_"
    REPLACEMENTS = {
        "!": "PLING",
        "$": "DOLLAR",
        ":": "COLON",
        "(": "LPAREN",
        "<": "LESS",
        ">": "GREATER",
        "=": "EQUAL",
        "\\": "BACKSLASH",
        ")": "RPAREN",
        "@": "ATCH",
        "[": "LSQPAREN",
        "]": "RSQPAREN",
        "^": "HAT",
        "+": "PLUS",
        "-": "MINUS",
        "*": "STAR",
        "/": "SLASH",
        "%": "PERCENT",
        "&": "AMPERSAND",
        "?": "QMARK",
        ";": "SEMICOLON",
        "'": "QUOTE",
        "`": "BQUOTE",
        "{": "LCURLY",
        "}": "RCURLY",
        "_": "UNDERSCORE",
        "|": "BAR",
        ",": "COMMA",
        "#": "HASH",
        ".": "PERIOD",
        '"': "DQUOTE",
        "~": "TILDE",
        " ": "SPACE",
    }

    @classmethod
    def desc(cls) -> str:
        raise NotImplementedError()

    @classmethod
    def token_sets(cls) -> list[int]:
        raise NotImplementedError()

    @classmethod
    def generate_all(cls) -> Sequence[Self]:
        raise NotImplementedError()

    @classmethod
    def replace_punct(cls, s: str) -> str:
        """Convert string with punctuation to  a valid identifier"""
        result = s
        for old, new in cls.REPLACEMENTS.items():
            result = result.replace(old, new)
        return result

    @classmethod
    def reserved_name(cls, id: int) -> str:
        return "{0}x{1:02X}".format(Token.RESERVED_PREFIX, id)

    def __init__(self, *, name: str, set: int, id: int | None = None):
        if id is not None:
            self._assert_id_in_range(id)

        self.name = name
        self.set = set
        self.id = id
        self.label = None

    def get_name(self) -> str:
        return self.name

    def is_reserved(self) -> bool:
        return self.name.startswith(Token.RESERVED_PREFIX)

    def get_set(self) -> int:
        return self.set

    def set_id(self, id: int):
        self._assert_id_in_range(id)
        self.id = id

    def has_id(self) -> bool:
        return self.id is not None

    def get_id(self) -> int:
        assert self.id is not None, "ID not set for token " + self.name
        return self.id

    def set_label(self, label: str):
        self.label = label

    def get_label(self) -> str | None:
        return self.label

    def get_asm_identifier(self) -> str:
        prefix = "KWD{0}".format(self.set) if self.set > 0 else "KWD"
        return "{0}_{1}".format(
            prefix, self.name if self.is_reserved() else Token.replace_punct(self.name)
        )

    def _assert_id_in_range(self, id: int):
        assert id in self.ID_RANGE, "Token ID {0} is not in {1}".format(
            id, self.ID_RANGE
        )


class MetaToken(Token):
    SET_ID: int
    ID_RANGE: range
    TOKENS: list[str] = []

    @classmethod
    def token_sets(cls) -> list[int]:
        return [cls.SET_ID]

    @classmethod
    def generate_all(cls) -> Sequence[Self]:
        set_id = cls.SET_ID
        result = []
        for i, name in enumerate(cls.TOKENS, start=cls.ID_RANGE.start):
            result.append(cls(name=name, set=set_id, id=i))

        for i in range(cls.ID_RANGE.start + len(result), cls.ID_RANGE.stop):
            result.append(cls(name=Token.reserved_name(i), set=set_id, id=i))

        return result

    def get_asm_identifier(self) -> str:
        return "KWC_{0}".format(self.name.replace("#", ""))


class CtrlToken(MetaToken):
    SET_ID = -3
    ID_RANGE = range(0, 8)
    TOKENS = [
        "#EOL",
        "#KWDSET1",
        "#KWDSET2",
    ]

    @classmethod
    def desc(cls):
        return "control tokens"


class PunctuationToken(Token):
    """
    Punctuation tokens fall into three distinct groups:

    1.  Double punctuation: `>` or `<` followed by `<`, `=`, or `>`, giving us 5 legal
        two-character elements: comparisons (`<=`, `>=`, `<>`) and shifts (`<<`, `>>`).

    2.  Normal punctuation: characters with ASCII codes 33-63 whose token IDs match
        their character codes.

    3.  Extended punctuation: characters from 64-127 mapped to range $10-$1F by
        taking the lower 3 bits (`i & 7`), shifting bit 5 down to bit 3 (`(i & 0x20) >> 2`),
        and ORing with $10.
    """

    SET_ID = -2
    ID_RANGE = range(8, 64)
    PRECEDENCE = {
        "&": 1,
        "|": 1,
        "^": 1,
        ">": 2,
        ">=": 2,
        "<": 2,
        "<=": 2,
        "=": 2,
        "<>": 2,
        "+": 3,
        "-": 3,
        "*": 4,
        "/": 4,
        "%": 4,
        "<<": 4,
        ">>": 4,
        "\\": 4,
        "!": 5,
        "?": 5,
        "$": 5,
    }

    @classmethod
    def desc(cls):
        return "punctuation tokens"

    @classmethod
    def token_sets(cls) -> list[int]:
        return [cls.SET_ID]

    #
    # Create all punctuation tokens. Punctuation tokens have predefined IDs.
    #
    @classmethod
    def generate_all(cls) -> Sequence[Self]:
        # see https://en.wikipedia.org/wiki/ASCII#Character_set

        # tokens by ID
        tokens: dict[int, str] = {}

        # assign first 8 IDs in the range to double punctuation tokens
        # (`<<`, `<=`, `<>`, `><`, `>=`, `>>`)
        index = 0
        for i in range(0, 2):
            for j in range(0, 3):
                index = cls.ID_RANGE.start + i * 4 + j
                tokens[index] = "<>"[i] + "<=>"[j]

        # 16 to 32 is reserved for extended punctuation tokens mixed in
        # with the alphas, see below
        assert index < 16, (
            "Punctuation ID range {0} does not have enough space for double punctuation tokens".format(
                cls.ID_RANGE
            )
        )

        # token IDs of punctuation chars with codes from 33 to 64 match
        # their character codes
        for i in range(33, 64):
            c = chr(i)
            if c < "0" or c > "9":  # exclude digits
                tokens[i] = c

        # the remaining extended punctuation characters with codes up to,
        # but not including 127 (which is a control character), are
        # assigned token IDs in the range 16 to 32
        for i in range(64, 127):
            c = chr(i).upper()
            if c < "A" or c > "Z":
                tokens[((i & 0x20) >> 2) | (i & 7) | 0x10] = c

        # create token objects for each token in the range
        result = []
        for i in cls.ID_RANGE:
            t = tokens.get(i)
            newToken = PunctuationToken(
                name=t if t is not None else Token.reserved_name(i)
            )
            newToken.set_id(i)
            newToken.set_precedence(cls.op_precedence(newToken.get_name()))
            result.append(newToken)

        return result

    #
    # Get operator precedence
    #
    @classmethod
    def op_precedence(cls, op):
        return cls.PRECEDENCE[op] if op in cls.PRECEDENCE else 0

    def __init__(self, *, name: str):
        super().__init__(name=name, set=PunctuationToken.SET_ID)
        self.precedence = 0

    def get_precedence(self):
        return self.precedence

    def set_precedence(self, p):
        self.precedence = p


class UserDefinedToken(Token):
    SET_ID = -1
    ID_RANGE = range(64, 128)

    @classmethod
    def desc(cls):
        return "user-defined variables and functions"

    @classmethod
    def token_sets(cls) -> list[int]:
        return [cls.SET_ID]

    @classmethod
    def generate_all(cls) -> list[Self]:
        return [cls(name=Token.reserved_name(i), id=i) for i in cls.ID_RANGE]

    def __init__(self, *, name: str, id: int):
        super().__init__(name=name, set=UserDefinedToken.SET_ID, id=id)


class KeywordToken(Token):
    ID_RANGE = range(128, 240)
    SET_RANGE = range(0, 3)

    # A mini-DSL for all the keywords, functions, assembly instructions, and
    # other builtin identifier tokens used in the language. Token definitions
    # are grouped using the following category markers:
    #
    #   {u} - Unary functions (set 0)
    #   {+} - Block-opening keywords that increase indentation level (set 0)
    #   {-} - Block-closing keywords that decrease indentation level (set 0)
    #   {0} - Standard keywords (set 0)
    #   {1} - Command keywords (set 1)
    #   {2} - Assembly language mnemonics (set 2)
    #
    # These groupings are used to drive tokenization and syntax highlighting.
    # Each token is assigned a numeric ID based on its category and position.
    TOKENS_DEF = """
    {u} 						// set 0, unary functions
        abs 		asc 		chr$ 		alloc		frac		len
        left$ 		mid$ 		right$ 		rnd 		sgn 		int
        spc 		str$ 		val  		isval		true 		false
        not			random	 	timer 		event 		joyx		joyy
        joyb 		min			max 		hit 		playing		gettime$
        peek 		peekw 		peekl		peekd		getdate$	inkey$
        get$ 		inkey		get 		itemcount	itemget$ 	keydown

    {+}							// set 0, block structures, shift up
        while
        if
        repeat
        for
        proc
    {-} 						// set 0, block structures, shift down
        wend
        endif
        then
        until
        next
        endproc

    {0}							// set 0, generic keywords
        data 		dim 		let 		rem  		else 		to
        downto		call 		read 		local 		line 		by
        sprite 		rect		text 		circle 		here 		color
        colour 		solid 		outline 	gfx			image 		at
        from		plot 		on 			off 		palette 	sound
        poke 		pokew 		pokel 		poked 		memcopy 	clear
        tab

    {1}							// set 1, generic keywords
        end 		new 		list 		run 		stop
        restore 	assert 		assemble 	bitmap		sprites
        load 		go 			zap	 		ping 		setdate
        shoot 		explode 	xload 		xgo 		settime
        save		verify		drive 		dir 		bload
        bsave		himem 		input 		cls 		gosub
        return 		print 		cprint 		goto 		cursor
        mouse 		mdelta 		try 		tile 		tiles
        option

    {2}							// set 2, assembler mnemonics
        adc	and	asl	bcc	bcs	beq	bit	bmi	bne	bpl	bra	brk	bvc	bvs
        clc	cld	cli	clv	cmp	cpx	cpy	dec	dex	dey	eor	inc	inx	iny
        jmp	jsr	lda	ldx	ldy	lsr	nop	ora	pha	php	phx	phy	pla	plp
        plx	ply	rol	ror	rti	rts	sbc	sec	sed	sei	sta	stx	sty	stz
        tax	tay	trb	tsb	tsx	txa	txs	tya stp
    """

    @classmethod
    def desc(cls):
        return "keywords & builtin functions"

    @classmethod
    def token_sets(cls) -> list[int]:
        return list(cls.SET_RANGE)

    #
    # Generate all alpha-numerical tokens and their IDs.
    #
    @classmethod
    def generate_all(cls) -> Sequence[Self]:
        tokens = cls.parse(cls.TOKENS_DEF)
        tokens.sort(key=lambda x: x.sort_key())  # sort before assigning IDs

        next_id = cls.allocate_ids(tokens=tokens, id_range=cls.ID_RANGE)

        default_set = cls.SET_RANGE.start
        for i in range(next_id[default_set], cls.ID_RANGE.stop):
            tokens.append(cls(name=Token.reserved_name(i), set=default_set, id=i))

        return tokens

    #
    # Parse tokens from the DSL
    #
    @classmethod
    def parse(cls, tokens_def: str) -> list[Self]:
        # split tokens DSL into lines, replace tabs w/ spaces,
        # convert to uppercase
        s = tokens_def.upper().replace("\t", " ").split("\n")

        # remove comments
        s = [x if x.find("//") < 0 else x[: x.find("//")] for x in s]

        # convert into list
        tokens = " ".join(s).split()

        token_sets: set[int] = set()  # collect token sets
        result = []
        category = "0"
        for w in tokens:
            if w.startswith("{") and w.endswith("}"):
                category = w[1:-1]
            else:
                if category == "+" or category == "-":  # block start/end
                    cls = BlockStartToken if category == "+" else BlockEndToken
                    newToken = cls(name=w)
                elif category == "U":  # unary function
                    newToken = UnaryToken(w)
                else:  # the rest
                    newToken = KeywordToken(name=w, set=int(category))

                token_sets.add(newToken.get_set())  # collect token sets
                result.append(newToken)

        assert sorted(list(token_sets)) == list(cls.SET_RANGE), (
            "Unexpected list of token sets " + str(token_sets)
        )

        return result

    #
    # Assign unique numeric identifiers to alpha-numeric tokens within
    # their respective token sets. Returns a dict of the next available
    # token ID per set.
    #
    @classmethod
    def allocate_ids(cls, *, tokens: list[Self], id_range: range) -> dict[int, int]:
        # tokens in different sets can have overlapping IDs
        next_token_id: dict[int, int] = {}
        for t in tokens:
            assert not t.has_id(), "Token {0} already has ID {1}".format(
                t.get_name(), t.get_id()
            )

            s = t.get_set()
            if s not in next_token_id:
                next_token_id[s] = id_range.start

            token_id = next_token_id[s]
            assert token_id < id_range.stop, "Token ID {0} for {1} exceeds {2}".format(
                t.get_id(), t.get_name(), id_range
            )

            t.set_id(token_id)
            next_token_id[s] += 1

        return next_token_id

    def sort_key(self) -> tuple[int, int, str]:
        return (self._sort_order(), self.set, self.name)

    def _sort_order(self) -> int:
        return 9


class UnaryToken(KeywordToken):
    SET_ID = 0

    def __init__(self, name: str):
        super().__init__(name=name, set=UnaryToken.SET_ID)

    def _sort_order(self) -> int:
        return 1


class BlockToken(KeywordToken):
    SET_ID = 0

    def __init__(self, *, name: str):
        super().__init__(name=name, set=BlockToken.SET_ID)


class BlockStartToken(BlockToken):
    def _sort_order(self) -> int:
        return 2


class BlockEndToken(BlockToken):
    def _sort_order(self) -> int:
        return 3


class DataToken(MetaToken):
    SET_ID = 255
    ID_RANGE = range(240, 256)
    TOKENS = [
        "#NONPRINTABLE",
        "#EXTASCII",
        "#STRING",
        "#HEXCONST",
        "#DECIMAL",
        "#COMMENT",
    ]

    @classmethod
    def desc(cls):
        return "data tokens"


# four main token categories are control tokens, punctuation,
# user-defined identifiers, and keywords
TOKEN_CATEGORIES: list[type[Token]] = [
    CtrlToken,
    PunctuationToken,
    UserDefinedToken,
    KeywordToken,
    DataToken,
]


def do_sets_overlap(sets: list[set]) -> bool:
    seen = set()
    for s in sets:
        if seen & s:
            return True
        seen |= s
    return False


# make sure that token ID ranges do not overlap with each other
assert not do_sets_overlap([set(cls.ID_RANGE) for cls in TOKEN_CATEGORIES]), (
    "Token ID ranges overlap:\n\t"
    + "\n\t".join(
        ["{0}: {1}".format(cls.__name__, cls.ID_RANGE) for cls in TOKEN_CATEGORIES]
    )
)

# make sure that token sets do not overlap with each other
assert not do_sets_overlap([set(cls.token_sets()) for cls in TOKEN_CATEGORIES]), (
    "Token sets overlap:\n\t"
    + "\n\t".join(
        ["{0}: {1}".format(cls.__name__, cls.token_sets()) for cls in TOKEN_CATEGORIES]
    )
)


#
# A collection of all the tokens in the language with methods to generate
# corresponding assembly directives, constants, lookup tables, etc.
#
class TokenCollection(object):
    tokens: dict[str, Token] = {}  # name -> token
    token_list: list[Token] = []  # sorted list of tokens

    def __init__(self):
        tokens = [t for cls in TOKEN_CATEGORIES for t in cls.generate_all()]
        for t in tokens:
            self.add_token(t.get_name(), t)

        self.link_to_labels()

    def add_token(self, w, newToken):
        assert w not in self.tokens, "Duplicate token " + w  # only once
        self.tokens[w] = newToken  # set up array/dictionary
        self.token_list.append(newToken)

    def get_token(self, w):
        w = w.upper().strip()
        return self.tokens[w] if w in self.tokens else None

    #
    # Generate `.text` directives section for the specified token set
    #
    def write_keyword_set(self, *, set: int, out: TextIO):
        out.write("KeywordSet{0}:\n".format(set))
        out.write("\t;\t\tlength,\thash,\tkeyword\t\t\t; token ID\ttoken name\n")
        for t in self.token_list:
            if t.get_set() == set and not t.is_reserved():
                hash = sum([ord(x) for x in t.get_name()]) & 0xFF
                name = "" if t.get_name().startswith("!") else t.get_name()
                out.write(
                    "\t.text\t{0},\t\t${1:02x},\t{2:16}; ${3:02x}\t\t{4}\n".format(
                        len(name), hash, '"' + name + '"', t.get_id(), t.get_name()
                    )
                )
        out.write("\t.text\t$FF\n")

    #
    # Generate assembly constants for the specified token set
    #
    def write_constants(self, *, set: int, out: TextIO):
        for t in self.token_list:
            if t.get_set() == set:
                out.write(
                    "{0:32} = ${1:02x}\t\t; ${1:02x} {2}\n".format(
                        t.get_asm_identifier(),
                        t.get_id(),
                        t.get_name(),
                    )
                )

    #
    # Generate a precedence table
    #
    def write_precedence_table(self, *, out: TextIO):
        out.write("PrecedenceLevel:\n")
        for t in self.token_list:
            if isinstance(t, PunctuationToken):
                out.write(
                    "\t.byte\t{0:2}\t; ${1:02x} {2}\n".format(
                        t.get_precedence(), t.get_id(), t.get_name()
                    )
                )

    #
    # Generate vector set
    #
    def write_vector_set(self, *, set, suffix, out):
        out.write("VectorSet{0}:\n".format(suffix))
        for t in self.token_list:
            if t.get_set() == set:
                label = t.get_label() if t.get_label() is not None else "SyntaxError"
                out.write(
                    "\t.word\t{0:32} ; ${1:02x} {2}\n".format(
                        label, t.get_id(), t.get_name()
                    )
                )

    def write_first_last_constants(self, *, out):
        def class_abbrev(cls: type[Token]) -> str:
            cls_name = cls.__name__
            return cls_name.replace("Token", "").upper()

        def write_marker(prefix: str, cls: type[Token], t: Token):
            out.write(
                "KWC_{0:28} = {1:32}; ${2:02x}\n".format(
                    prefix + "_" + class_abbrev(cls), t.get_asm_identifier(), t.get_id()
                )
            )

        def write_first(cls: type[Token], t: Token):
            write_marker("FIRST", cls, t)

        def write_last(cls: type[Token], t: Token):
            write_marker("LAST", cls, t)

        def matching_class(
            t: Token, token_classes: list[type[Token]]
        ) -> type[Token] | None:
            matching_classes = [cls for cls in token_classes if isinstance(t, cls)]
            return matching_classes[0] if len(matching_classes) > 0 else None

        def write_first_last(token_classes: list[type[Token]]):
            last: Token | None = None
            last_cls: type[Token] | None = None
            for t in self.token_list:
                if t.get_set() > 0 and not isinstance(t, MetaToken):
                    continue

                cls = matching_class(t, token_classes)
                if last_cls != cls:
                    if last is not None and last_cls is not None:
                        write_last(last_cls, last)

                    if cls is not None:
                        write_first(cls, t)
                    last_cls = cls

                last = t

            if last is not None and last_cls is not None:
                write_last(last_cls, last)

        write_first_last(
            [
                BlockStartToken,
                BlockEndToken,
                UnaryToken,
                UserDefinedToken,
                PunctuationToken,
                CtrlToken,
            ]
        )

        write_first_last([BlockToken])
        write_first_last([KeywordToken, DataToken])

    #
    # Scan the source to associate subroutine implementations marked with a
    # `;; [token]` comment with their corresponding tokens
    #
    def link_to_labels(self):
        label_re = re.compile(r"^(\w+):\s+;;\s+\[(.*?)\]\s*$")

        def has_include(line: str) -> bool:
            return line.strip().startswith(".include")

        def process_include(filename: str):
            with open(filename, "r") as f:
                for line in f:
                    process_line(line)

        def process_line(line: str):
            if m := label_re.match(line):
                label = m.group(1).strip()
                token = m.group(2).strip().upper()
                assert token in self.tokens, (
                    "Attempt to link to an unknown token '{0}'".format(token)
                )

                t = self.tokens[token]
                assert t.get_label() is None, (
                    "Token '{0}' has already been linked to '{1}'".format(
                        token, t.get_label()
                    )
                )

                print("-> Linking token '{0}' to label '{1}'".format(token, label))
                t.set_label(label)

        with open("_basic.asm", "r") as src:
            for line in src:
                if has_include(line):
                    filename = line.split('"')[1]
                    process_include(filename)


if __name__ == "__main__":
    note = ";\n;\tThis is automatically generated.\n;\n"
    out_dir = Path(".") / "common" / "generated"

    t = TokenCollection()

    with open(out_dir / "kwdtext.dat", "w") as kwd_text:
        kwd_text.write(note)
        kwd_text.write("\t.section code\n")
        for s in KeywordToken.SET_RANGE:
            t.write_keyword_set(set=s, out=kwd_text)
        kwd_text.write("\t.send code\n")

    with open(out_dir / "kwdconst.inc", "w") as kwd_const:
        kwd_const.write(note)
        for token_cls in TOKEN_CATEGORIES:
            token_sets = token_cls.token_sets()
            for s in token_sets:
                kwd_const.write(
                    ";; {0}, set {1}\n".format(token_cls.desc(), s)
                    if len(token_sets) > 1
                    else ";; {0}\n".format(token_cls.desc())
                )
                t.write_constants(set=s, out=kwd_const)

        kwd_const.write("\n;\n; first & last category markers\n;\n")
        t.write_first_last_constants(out=kwd_const)

    with open(out_dir / "precedence.dat", "w") as prec:
        prec.write(note)
        t.write_precedence_table(out=prec)

    with open(out_dir / "vectors.dat", "w") as vec:
        vec.write(note)
        t.write_vector_set(set=PunctuationToken.SET_ID, suffix="Punc", out=vec)
        for s in KeywordToken.SET_RANGE:
            t.write_vector_set(set=s, suffix=str(s), out=vec)
