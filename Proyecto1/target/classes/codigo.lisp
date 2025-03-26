(setq a 10)
(setq b 6)
(setq c 5)
(cond
((>= a b)
(cond
((>= a c) (print a))
(t (print c))))
((>= b c) (print b))
(t (print c)))