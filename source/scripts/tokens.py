from typing import Self, Sequence, TextIO
import re

from pathlib import Path


class Token(object):
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

    def __init__(self, *, name: str, set: int, id: int | None = None):
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


class CtrlToken(Token):
    SET_ID = -2
    ID_RANGE = range(0, 8)
    TOKENS = ["_EOL", "_SHIFT1", "_SHIFT2", "_STRING", "_HEXCONST", "_DECIMAL"]

    @classmethod
    def desc(cls):
        return "control tokens"

    @classmethod
    def token_sets(cls) -> list[int]:
        return [cls.SET_ID]

    @classmethod
    def generate_all(cls) -> Sequence[Self]:
        result = []
        for i, name in enumerate(cls.TOKENS, start=cls.ID_RANGE.start):
            result.append(CtrlToken(name=name, id=i))
        return result

    def __init__(self, *, name: str, id: int):
        super().__init__(name=name, set=CtrlToken.SET_ID, id=id)

    def get_asm_identifier(self) -> str:
        return "KWC{0}".format(self.name)


class PunctuationToken(Token):
    SET_ID = -1
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

        # 16 to 32 is reserved for single punctuation tokens mixed in
        # with the alphas, see below
        assert index < 16, (
            "Punctuation ID range {0} does not have enough space for double punctuation tokens".format(
                cls.ID_RANGE
            )
        )

        # token IDs of single punctuation chars with codes from 33 to 64
        # match their character codes
        for i in range(33, 64):
            c = chr(i)
            if c < "0" or c > "9":  # exclude digits
                tokens[i] = c

        # the remaining ASCII punctuation characters with codes up to,
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
                name=t
                if t is not None
                else "{0}x{1:02x}".format(Token.RESERVED_PREFIX, i)
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


class KeywordToken(Token):
    ID_RANGE = range(128, 256)
    SET_RANGE = range(0, 3)

    # A mini-DSL for all the keywords, functions, assembly instructions, and
    # other builtin identifier tokens used in the language. Token definitions
    # are grouped using the following category markers:
    #
    #   {+} - Structure-opening keywords that increase indentation level
    #   {-} - Structure-closing keywords that decrease indentation level
    #   {u} - Unary functions
    #   {0} - Standard keywords (set 0)
    #   {1} - Command keywords (set 1)
    #   {2} - Assembly language mnemonics (set 2)
    #
    # These groupings are used to drive tokenization and syntax highlighting.
    # Each token is assigned a numeric ID based on its category and position.
    TOKENS_DEF = """
    {+}							// Control structures, shift up
        while
        if
        repeat
        for
        proc
    {-} 						// Control structures, shift down
        wend
        endif
        then
        until
        next
        endproc
    {u} 						// Unary functions
        abs 		asc 		chr$ 		alloc		frac		len
        left$ 		mid$ 		right$ 		rnd 		sgn 		int
        spc 		str$ 		val  		isval		true 		false
        not			random	 	timer 		event 		joyx		joyy
        joyb 		min			max 		hit 		playing		gettime$
        peek 		peekw 		peekl		peekd		getdate$	inkey$
        get$ 		inkey		get 		itemcount	itemget$ 	keydown

    {0}							// Set 0
        data 		dim 		let 		rem  		else 		to
        downto		call 		read 		local 		line 		by
        sprite 		rect		text 		circle 		here 		color
        colour 		solid 		outline 	gfx			image 		at
        from		plot 		on 			off 		palette 	sound
        poke 		pokew 		pokel 		poked 		memcopy 	clear
        tab

    {1}							// Set 1
        end 		new 		list 		run 		stop
        restore 	assert 		assemble 	bitmap		sprites
        load 		go 			zap	 		ping 		setdate
        shoot 		explode 	xload 		xgo 		settime
        save		verify		drive 		dir 		bload
        bsave		himem 		input 		cls 		gosub
        return 		print 		cprint 		goto 		cursor
        mouse 		mdelta 		try 		tile 		tiles
        option

    {2}							// Set 2 (Assembler Mnemonics)
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

        cls.allocate_ids(tokens=tokens, id_range=KeywordToken.ID_RANGE)

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
                if category == "+" or category == "-":  # adjuster / structure
                    cls = StructStartToken if category == "+" else StructEndToken
                    newToken = cls(name=w)
                elif category == "U":  # unary function
                    newToken = UnaryToken(w)
                else:  # The rest
                    newToken = KeywordToken(name=w, set=int(category))

                token_sets.add(newToken.get_set())  # collect token sets
                result.append(newToken)

        assert sorted(list(token_sets)) == list(cls.SET_RANGE), (
            "Unexpected list of token sets " + str(token_sets)
        )

        return result

    #
    # Assign unique numeric identifiers to alpha-numeric tokens within
    # their respective token sets.
    #
    @classmethod
    def allocate_ids(cls, *, tokens: list[Self], id_range: range):
        # tokens in different sets can have overlapping IDs
        next_token_id = {}
        for t in tokens:
            assert not t.has_id(), "Token {0} already has ID {1}".format(
                t.get_name(), t.get_id()
            )

            s = t.get_set()
            if s not in next_token_id:
                next_token_id[s] = id_range.start

            token_id = next_token_id[s]
            assert token_id <= id_range.stop, (
                "Token ID {0} for {1} exceeds range {2}".format(
                    t.get_id(), t.get_name(), id_range
                )
            )

            t.set_id(token_id)
            next_token_id[s] += 1

    def sort_key(self) -> tuple[int, int, str]:
        return (0, self.set, self.name)


class StructToken(KeywordToken):
    SET_ID = 0

    def __init__(self, *, name: str):
        super().__init__(name=name, set=StructToken.SET_ID)


class StructStartToken(StructToken):
    def sort_key(self):
        return (2, self.set, self.name)


class StructEndToken(StructToken):
    def sort_key(self):
        return (3, self.set, self.name)


class UnaryToken(KeywordToken):
    SET_ID = 0

    def __init__(self, name: str):
        super().__init__(name=name, set=UnaryToken.SET_ID)

    def sort_key(self):
        return (1, self.set, self.name)


TOKEN_CLASSES: list[type[Token]] = [CtrlToken, PunctuationToken, KeywordToken]


#
# A collection of all the tokens in the language with methods to generate
# corresponding assembly directives, constants, lookup tables, etc.
#
class TokenCollection(object):
    tokens: dict[str, Token] = {}  # name -> token
    token_list: list[Token] = []  # sorted list of tokens

    def __init__(self):
        tokens = [t for cls in TOKEN_CLASSES for t in cls.generate_all()]
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
            if t.get_set() == set:
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
                "KWC_{0:28} = {1}\n".format(
                    prefix + "_" + class_abbrev(cls), t.get_asm_identifier()
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
                if t.get_set() > 0:
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
                StructStartToken,
                StructEndToken,
                UnaryToken,
                PunctuationToken,
                CtrlToken,
            ]
        )

        write_first_last([StructToken])
        write_first_last([KeywordToken])

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
        for token_cls in TOKEN_CLASSES:
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
