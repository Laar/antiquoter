# Antiquoter

A combinator library making quasi/anti-quoting simpler. The problem with
writing antiquoters is that you have to write simmilar code for expression and
pattern context. This library makes it possible write generalized quasiquoters
that work both in expression and pattern contexts without dupplicating
antiquoters.
