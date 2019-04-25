#lang brag
spec-program : spec-line*
spec-line : ((spec-start | spec-state | spec-end) spec-name (spec-state | spec-end))
spec-start : START-TOK
spec-state : STATE-TOK
spec-end : END-TOK
spec-name : NAME-TOK