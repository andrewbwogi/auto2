#lang spec

=>(q0)-eps->(q0)
(q0)-Vote-submit->(q0)
(q0)-Vote-validate->(q0)
(q0)-Vote-main->(q0)
(q0)-Vote-vote->(q1)
(q1)-eps->(q1)
(q1)-Vote-submit->(q1)
(q1)-Vote-main->(q1)
(q1)-Vote-vote->(q1)
(q1)-Vote-getVote->(q1)
(q1)-Vote-validate->(q1)
(q0)-Vote-getVote->[q2]
[q2]-eps->[q2]
[q2]-Vote-submit->[q2]
[q2]-Vote-main->[q2]
[q2]-Vote-vote->[q2]
[q2]-Vote-getVote->[q2]
[q2]-Vote-validate->[q2]
