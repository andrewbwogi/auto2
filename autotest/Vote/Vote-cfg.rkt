#lang sys

node v0 meth(main) entry
node v1 meth(main) ret
node v2 meth(vote) entry
node v3 meth(vote) none
node v4 meth(vote) ret
node v5 meth(vote) ret
node v6 meth(getVote) entry
node v7 meth(getVote) ret
node v8 meth(validate) entry
node v9 meth(validate) ret
node v10 meth(submit) entry
node v11 meth(submit) ret
edge v0 v1 vote
edge v2 v3 getVote
edge v3 v4 submit
edge v3 v5 vote
edge v6 v7 validate
edge v8 v9 eps
edge v10 v11 eps
