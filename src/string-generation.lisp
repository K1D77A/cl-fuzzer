;;;;this file contains all the functions required to generate a random list that will
;;;;be passed to a function that jams it down a stream

(in-package :cl-fuzzer)

(defparameter *test-input*
  '((:string "start")(:repeat 3 "ab32a")(:repeat 24 :nul)(:repeat 10 :LF)(:upto 1 25)(:downto 25 0)
    (:char 1 #\Newline)(:random-string 5)(:string "stop")))

;;;Important note that all functions return lists of characters/numbers

(defparameter *special-asciis*
  ;;generated programmatically but the keyword names are the official ascii ones from
  ;;https://www.asciitable.com/
  '((:NUL #\Nul) (:SOH #\Soh) (:STX #\Stx) (:ETX #\Etx) (:EOT #\Eot) (:ENQ #\Enq)
    (:ACK #\Ack) (:BEL #\Bel) (:BS #\Backspace) (:TAB #\Tab) (:LF #\Newline)
    (:VT #\Vt) (:FF #\Page) (:CR #\Return) (:SO #\So) (:SI #\Si) (:DLE #\Dle)
    (:DC1 #\Dc1) (:DC3 #\Dc2) (:DC3 #\Dc3) (:DC4 #\Dc4) (:NAK #\Nak) (:SYN #\Syn)
    (:ETB #\Etb) (:CAN #\Can) (:EM #\Em) (:SUB #\Sub) (:ESC #\Esc) (:FS #\Fs)
    (:GS #\Gs) (:RS #\Rs) (:US #\Us) (:SPACE #\ )(:DEL #\Rubout)))

(defun random-string-list (len)
  "creates a string of random contents generated from the alphabet of length len. and 
returns it as a list of char-codes"
  (let ((chars "abcdefghijklmnopqrstuvwxyz"))
    (map 'list #'char-code
         (loop :for x :from 0 :below len
               :collect (aref chars (random 26))))))

(defun string-repeat (n string)
  "repeats string n times and converts to a list of char-codes"
  (let ((str ""))
    (map 'list #'char-code
         (dotimes (x n str)
           (setf str (concatenate 'string str string))))))

(defun special-char-repeat (n char)
  (string-repeat n (string char)))

(defun n-upto-x (n x)
  "Generates the numbers from n upto x. n must be greater than 0 and n must be less than 255"
  (if (and (>= n 0) (<= x 255) (< n x))
      (loop :for i :from n :upto x
            :collect i)
      (error "N is less than 0 or X is greater than 255")))

(defun x-downto-n (x n)
  "Generates the numbers from x downto n. n must be greater than 0 and n must be less than 255"
  (if (and (>= x 0) (<= n 255)(> x n))
      (loop :for i :from x :downto n
            :collect i)
      (error "N is less than 0 or X is greater than 255")))

(defun handle-repeat (form)
  (destructuring-bind (n repeat)
      form
    (if (keywordp repeat)
        (special-char-repeat n (second  (assoc repeat *special-asciis*)))
        (string-repeat n repeat))))

(defun handle-upto (form)
  (destructuring-bind (x y)
      form
    (n-upto-x x y)))

(defun handle-downto (form)
  (destructuring-bind (x y)
      form
    (x-downto-n x y)))

(defun handle-string (form)
  (destructuring-bind (string)
      form
    (string-repeat 1 string)))

(defun handle-char (form)
  (destructuring-bind (n char)
      form
    (string-repeat n (string char))))

(defun handle-random-string (form)
  (destructuring-bind (n)
      form
    (random-string-list n)))

(defun generate-fuzzy-list (form)
  "Creates a list from form of char-codes. Takes in an alist of keywords (for an example see 
*test-input*) and outputs a list of all the char-codes in one long list"
  (reduce #'append 
          (mapcar (lambda (form)
                    (let ((command (first form)))
                      (case command
                        (:string (handle-string (rest form)))
                        (:char (handle-char (rest form)))
                        (:repeat (handle-repeat (rest form)))
                        (:upto (handle-upto (rest form)))
                        (:downto (handle-downto (rest form)))
                        (:random-string (handle-random-string (rest form))))))
                  form)
          :from-end t))

(defun fuzzy-list-to-fuzzy-string (fuzzy-list)
  "takes in a fuzzy list and returns a string, however any null bytes will just be converted to nil
so its mostly just for aesthetic reasons"
  (map 'string #'code-char fuzzy-list))



