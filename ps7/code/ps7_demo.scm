(load "load")
(load "ps7_code")
;;; Problem 7.1
; Create two people
(people-ify 'alice 'bob)
; Fill out properties of 'alice
(tell! (eq-get 'alice 'height) 55 'dr_joe)
(tell! (eq-get 'alice 'weight) 145 'dr_joe)
(tell! (eq-get 'alice 'age) 14 'dr_joe)
(tell! (eq-get 'alice 'income) 95 'irs)
(tell! (eq-get 'alice 'expenses) 200 'irs)
; Fill out properties of 'bob
(tell! (eq-get 'bob 'height) 75 'dr_joe)
(tell! (eq-get 'bob 'weight) 345 'dr_joe)
(tell! (eq-get 'bob 'age) 64 'dr_joe)
(tell! (eq-get 'bob 'income) 25 'irs)
(tell! (eq-get 'bob 'expenses) 20 'irs)

;; Assert on 'Alice
; height assertions
(assert (contingent-info (inquire ((eq-path '(short height)) 'alice)))
	"'alice should be short")
(assert (not (contingent-info (inquire ((eq-path '(tall height)) 'alice))))
	"'alice should not be tall")
; weight assertions
(assert (not (contingent-info (inquire ((eq-path '(chubby weight)) 'alice))))
	"'alice should not be chubby")
(assert (not (contingent-info (inquire ((eq-path '(slender weight)) 'alice))))
	"'alice should not be slender")
(assert (contingent-info (inquire ((eq-path '(healthy weight)) 'alice)))
	"'alice should be healthy")
; age assertions
(assert (not (contingent-info (inquire ((eq-path '(old age)) 'alice))))
	"'alice should not be old")
(assert (not (contingent-info (inquire ((eq-path '(baby age)) 'alice))))
	"'alice should not be a baby")
(assert (not (contingent-info (inquire ((eq-path '(child age)) 'alice))))
	"'alice should not be a child")
(assert (contingent-info (inquire ((eq-path '(teen age)) 'alice)))
	"'alice should be a teen")
(assert (contingent-info (inquire ((eq-path '(ya age)) 'alice)))
	"'alice should be a young adult")
(assert (not (contingent-info (inquire ((eq-path '(adult age)) 'alice))))
	"'alice should not be an adult")
; income assertions
(assert (not (contingent-info (inquire ((eq-path '(rich income)) 'alice))))
	"'alice should not be rich")
(assert (not (contingent-info (inquire ((eq-path '(poor income)) 'alice))))
	"'alice should not be poor")
(assert (contingent-info (inquire ((eq-path '(middle-class income)) 'alice)))
	"'alice should be middle class")
; expenses assertions
(assert (contingent-info (inquire ((eq-path '(spender expenses)) 'alice)))
	"'alice should be a spender")
(assert (not (contingent-info (inquire ((eq-path '(miser expenses)) 'alice))))
	"'alice should not be a miser")
; complicated assertions
(assert (contingent-info (inquire (eq-get 'alice 'living-beyond-means)))
	"'alice should be living beyond her means")
(assert (not (contingent-info (inquire (eq-get 'alice 'profligate))))
	"'alice should not be profligate")
(assert (not (contingent-info (inquire (eq-get 'alice 'frugal))))
	"'alice should not be frugal")
(assert (not (contingent-info (inquire (eq-get 'alice 'obese))))
	"'alice should not be obese")
(assert (not (contingent-info (inquire (eq-get 'alice 'wealthy))))
	"'alice should not be wealthy")

;; Assert on 'bob
; height assertions
(assert (not (contingent-info (inquire ((eq-path '(short height)) 'bob))))
	"'bob should not be short")
(assert (contingent-info (inquire ((eq-path '(tall height)) 'bob)))
	"'bob should be tall")
; weight assertions
(assert (contingent-info (inquire ((eq-path '(chubby weight)) 'bob)))
	"'bob should be chubby")
(assert (not (contingent-info (inquire ((eq-path '(slender weight)) 'bob))))
	"'bob should not be slender")
(assert (not (contingent-info (inquire ((eq-path '(healthy weight)) 'bob))))
	"'bob should not be healthy")
; age assertions
(assert (contingent-info (inquire ((eq-path '(old age)) 'bob)))
	"'bob should be old")
(assert (not (contingent-info (inquire ((eq-path '(baby age)) 'bob))))
	"'bob should not be a baby")
(assert (not (contingent-info (inquire ((eq-path '(child age)) 'bob))))
	"'bob should not be a child")
(assert (not (contingent-info (inquire ((eq-path '(teen age)) 'bob))))
	"'bob should not be a teen")
(assert (not (contingent-info (inquire ((eq-path '(ya age)) 'bob))))
	"'bob should not be a young adult")
(assert (contingent-info (inquire ((eq-path '(adult age)) 'bob)))
	"'bob should be an adult")
; income assertions
(assert (not (contingent-info (inquire ((eq-path '(rich income)) 'bob))))
	"'bob should not be rich")
(assert (contingent-info (inquire ((eq-path '(poor income)) 'bob)))
	"'bob should be poor")
(assert (not (contingent-info (inquire ((eq-path '(middle-class income)) 'bob))))
	"'bob should not be middle class")
; expenses assertions
(assert (not (contingent-info (inquire ((eq-path '(spender expenses)) 'bob))))
	"'bob should not be a spender")
(assert (contingent-info (inquire ((eq-path '(miser expenses)) 'bob)))
	"'bob should not be a miser")
; complicated assertions
(assert (not (contingent-info (inquire (eq-get 'bob 'living-beyond-means))))
	"'bob should not be living beyond his means")
(assert (not (contingent-info (inquire (eq-get 'bob 'profligate))))
	"'bob should not be profligate")
(assert (contingent-info (inquire (eq-get 'bob 'frugal)))
	"'bob should be frugal")
(assert (contingent-info (inquire (eq-get 'bob 'obese)))
	"'bob should be obese")
(assert (not (contingent-info (inquire (eq-get 'bob 'wealthy))))
	"'bob should not be wealthy")

;; Test backwards propogation
(people-ify 'charles)
(tell! (eq-get 'charles 'wealthy) #t 'alice)
(inquire (eq-get 'charles 'income))
;Value: #(value=#[interval 250 10000],
;   premises=(self),
;   informants=((switch:p status-cell range)))

'Passed
