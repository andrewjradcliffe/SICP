(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))
;; from Ex. 4.63, used in Ex. 4.69
(assert! (rule (son-by-wife ?m ?s)
               (and (wife ?m ?w)
                    (son ?w ?s))))
(assert! (rule (son-of ?m ?s)
               (or (son ?m ?s)
                   (son-by-wife ?m ?s))))
(assert! (rule (grandson ?g ?s)
               (and (son-of ?g ?f)
                    (son-of ?f ?s))))
