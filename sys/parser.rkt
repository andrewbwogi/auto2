#lang brag
spec-program : spec-line*
spec-line : (node | edge)
node: NODE-TOK VAL-TOK METH-TOK VAL-TOK  
edge: EDGE-TOK VAL-TOK VAL-TOK VAL-TOK
