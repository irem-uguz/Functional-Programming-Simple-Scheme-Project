#lang scheme
;2015400165
;Irem Uğuz


;find-city is the recursive funtion that tries to find the given city in the given cities list.
;find-city return the list of the list cities that city is in or empty list if it is not found. 
(define (find-a a a-list func)
  (if (= (length a-list) 0)
      (list)
      ;If the cities is an empty list, it returns empty list.
      (if (= (length a-list) 1)
          ;If the cities consist only one element, checks if city is in that one element.
          (if (func a a-list) (car a-list) (list))
          ;If cities has more than one elements, it checks the first element and the rest recursively.
          (if (func a a-list) (car a-list) (find-a a (cdr a-list) func)))))

;RAILWAY-CONNECTION finds the city's list in LOCATIONS and returns this list's third element
(define (RAILWAY-CONNECTION city) (let ((x (find-a city LOCATIONS (lambda (city city-list) (equal? (car (car city-list)) city)))))
 (if (= (length x) 0) (list) (third x))))


;ACCOMMODATION-COST finds the city's list in LOCATIONS and returns this list's second element
(define (ACCOMMODATION-COST city) (let ((x (find-a city LOCATIONS (lambda (city city-list) (equal? (car (car city-list)) city))))) 
(if (= (length x) 0) (list) (second x))))

;INTERESTED-CITIES fins traveler's list using find-a function then returns its second element.
(define (INTERESTED-CITIES trav)
  (let ((x (find-a trav TRAVELERS (lambda (traveler traveler-list) (equal? (car (car traveler-list)) traveler)) )))
        (first(cdr x))))

;INTERESTED-ACTIVITIES finds traveler's list using find-traveller function then returns its third element.
(define (INTERESTED-ACTIVITIES trav)
  (let ((x (find-a trav TRAVELERS (lambda (traveler traveler-list) (equal? (car (car traveler-list)) traveler)) )))
        (first(cdr(cdr x)))))

;HOME finds traveler's list using find-traveller function then returns its last element.
(define (HOME trav)
  (let ((x (find-a trav TRAVELERS (lambda (traveler traveler-list) (equal? (car (car traveler-list)) traveler)) )))
        (fourth x)))

;traveler-from function is a base function that finds travelers that fits the condition f.
(define (traveler-from city travelers f) ( cond ((= (length travelers) 0) (list))
                                              ((= (length travelers) 1) (if (f city travelers)
;if there is only one element in travelers, it is checked and appended if it fits f, else the traveler-from is called for the empty list.
                                              (append (list (first (car travelers))) (traveler-from city (cdr travelers) f))
                                              (traveler-from city (cdr travelers) f)) )
;if there are more than one element in travelers, city is checked and appended else, the rest is given to traveler-from
                                              (else (if (f city travelers)
                                              (append (list (first (car travelers))) (traveler-from city (cdr travelers) f))
                                              (traveler-from city (cdr travelers) f)))))

;TRAVELER-FROM calls the recursive function traveler-from with function f that checks if that traveler is from given city
(define (TRAVELER-FROM city) (traveler-from city TRAVELERS (lambda (city trav-list) (equal? city (fourth(car trav-list))))))

;INTERESTED-IN-CITY call the recursive function traveler-from with function f that checks if that traveler is interedted in city.
(define (INTERESTED-IN-CITY city) (traveler-from city TRAVELERS (lambda (city trav-list) (member city (second (car trav-list))))))

;INTERESTED-IN-ACTIVITY call the recursive function traveler-from with function f that checks if that traveler is interedted in activity.
(define (INTERESTED-IN-ACTIVITY activity) (traveler-from activity TRAVELERS (lambda (activity acty-list) 
(member activity (third (car acty-list))))))

;remove-duplicates is a recursive function that takes a list then removes the duplicates in it.
(define (remove-duplicates l) (cond
                                ;if given list is null, then returned list is null.
                                ((null? l) '())
                                ;if the first member of l is also in the rest of it, that means it is duplicate
                                ((member (car l) (cdr l)) (remove-duplicates (cdr l)))
                                (else (cons (car l) (remove-duplicates (cdr l))))))

;append-check appends three lists and removes the duplicates from the result
(define (append-check xs ys xz )
  (remove-duplicates (append xs ys xz))) 

;RAILWAY-NETWORK function calls find-network with the list that consists of the connected cities to city and city. Then city is deleted from list.
(define (RAILWAY-NETWORK city) (if (null? (RAILWAY-CONNECTION city))
                                   (list)
                                   (cdr(find-network (append (RAILWAY-CONNECTION city) (list city)) city)))) 

;find-network takes a list and rotates it according to first-city recursively while addind the connected cities to it so it finds all connected cities.
(define (find-network city-list first-city) (if (= (length city-list) 0) (list)
                                                (if (equal? (car city-list) first-city)
                                                    city-list
                                                    (find-network (append-check (RAILWAY-CONNECTION  (car city-list)) (cdr city-list) (list(car city-list))) first-city))))
;(let ((x (fourth (find-a city LOCATIONS (lambda (city city-list) (equal? (car (car city-list)) city))))))
(define (interested-acty acty-list city-list) (cond ((null? acty-list) #f)
                                               ((member (car acty-list) city-list) #t)
                                               (else (interested-acty (cdr acty-list) city-list))))

;ACCOMMODATION-EXPENSES is 0 if location is traveler's home, else if it contains INTERESTED-ACTIVITIES 3 times cost, otherwise just cost.
(define (ACCOMMODATION-EXPENSES traveler location) (if (equal? (HOME traveler) location) 0
                                                       (if (interested-acty (INTERESTED-ACTIVITIES traveler)
                                                       (fourth (find-a location LOCATIONS (lambda (city city-list) (equal? (car (car city-list)) city)))))
                                                       (* 3 (ACCOMMODATION-COST location))
                                                       (ACCOMMODATION-COST location))))

;TRAVEL-EXPENSES is zero if location is traveler's home, otherwise if it is on railway connection 100, 200 if it is not
(define (TRAVEL-EXPENSES traveler location) (let ((x (HOME traveler)))
                                              (if (equal? x location) 0
                                                  (if (member location (RAILWAY-NETWORK x))
                                                      100
                                                      200))))

;EXPENSES is the sum of ACCOMMODATION-EXPENSES and TRAVEL-EXPENSES of given location for given traveler.
(define (EXPENSES traveler location) (+ (ACCOMMODATION-EXPENSES traveler location) (TRAVEL-EXPENSES traveler location)))

;smaller takes a list and a money and returns the list of cities that ACCOMODATION-COST is bigger than money. It is recursive. 
(define (smaller money cities) (if (null? cities) '()
                                       (if (<= money (ACCOMMODATION-COST (car (car cities))))
                                           (append (list (car (car cities))) (smaller money (cdr cities)))
                                           (smaller money (cdr cities))
                                           )))

;bigger takes a list and a monet limit, then finds the cities in that list whose ACCOMODATION-COST is smaller than money recursively.
(define (bigger money cities) (if (null? cities) '()
                                       (if (>= money (ACCOMMODATION-COST (car cities)))
                                           (append (list (car cities)) (bigger money (cdr cities)))
                                           (bigger money (cdr cities))
                                           )))

;IN-BETWEEN finds the cities that cost is between small and big using bigger function with the resulting list from smaller function.
(define (IN-BETWEEN small big) (bigger big (smaller small LOCATIONS)))

                                                  


