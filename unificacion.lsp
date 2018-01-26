

(defun unificacion(e1 e2)
	(if (eql (first e1) (first e2))
		(unificar (rest e1) (rest e2))
	)
)

(defun variable (a)
	(cond 
		((atom a) NIL)
		((eq (first a) '?) T)
	)
)

(defun atomo(at)
	(cond
		((atom at) T)
		((variable at) T)
		(T NIL)))
		

(defun aparece(b c)	
	(cond 
		((equal b c) T)
		((atomo c) NIL)
		((equal b (first c)) T)		
		((not (atomo (first c))) (aparece b (first c)))
		((eq (first c) NIL) NIL)
		(T  (aparece b (rest c))))
)


(defun intercambiar(e y q) 
	(cond
		((null  q) q)
		((equal y (first q)) (cons e (intercambiar e y (rest q))))
		((not (atomo (first q))) (cons (intercambiar e y (first q)) (intercambiar e y (rest q))))
		(T (cons (first q) (intercambiar e y (rest q))))
	)
)


(defun aplicar (z x)  
	(cond
		((null z) x)
		((eq (rest z) NIL) (intercambiar (first (first z)) (first (rest(first z))) x))
		(T (aplicar (rest z) (intercambiar (first (first z)) (first (rest(first z))) x)))
	)
	
)




(defun UNIFICAR(E1 E2)
	 (cond 
		((atomo E1) 
			 (cond
				((equal E1 E2) NIL)
				((variable E1) 
					(cond
						((aparece E1 E2) 'FALLO)
						(T (list (list E2 E1)))))

				((variable E2) (list (list E1 E2)))
				(T 'FALLO)
			))
			
		((atomo E2) 
		
			(cond
				((equal E1 E2) NIL)
				((variable E2) 
					(cond
						((aparece E2 E1) 'FALLO)
						(T (list (list E1 E2)))))
							
				((variable E1) (list (list E2 E1)))
				(T 'FALLO)
			))	
	
		( T (let ((T1) (T2) (Z1) (Z2))

			(setq T1 (rest E1))
			(setq T2 (rest E2))
			(setq Z1 (unificar (first E1) (first E2)))

			(cond
				((eq Z1 'FALLO) 'FALLO)
				(T 
					(setq G1 (aplicar Z1 T1))
					(setq G2 (aplicar Z1 T2))
					(setq Z2 (unificar G1 G2))
					(cond
						((eq Z2 'FALLO)  'FALLO)
						(T(append Z1 Z2))
					)
				)
			)
		))
	)
)
