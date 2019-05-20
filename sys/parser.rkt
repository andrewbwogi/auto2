#lang brag
sys-program : sys-line*
sys-line : (node | edge)
node: NODE-TOK VAL-TOK METH-TOK VAL-TOK  
edge: EDGE-TOK VAL-TOK VAL-TOK VAL-TOK