#lang sys

node v0 meth(main) entry
node v1 meth(main) ret
node v2 meth(main) ret
node v3 meth(a) entry
node v4 meth(a) ret
node v5 meth(a) ret
edge v0 v1 eps
edge v0 v2 a
edge v3 v4 eps
edge v3 v5 a