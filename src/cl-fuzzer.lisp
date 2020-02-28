;;;; cl-fuzzer.lisp
(in-package #:cl-fuzzer)
(defclass connection ()
  ((ip
    :accessor ip
    :initarg :ip)
   (port
    :accessor port
    :initarg :port)
   (socket
    :accessor socket)
   (socket-stream
    :accessor socket-stream)))
(defun make-connection (ip port)
  (make-instance 'connection :ip ip :port port))
(defmethod connect-to-remote-tcp ((connection connection))
  (with-accessors ((ip ip)
                   (port port))
      connection
    (let* ((sock (usocket:socket-connect ip port :element-type '(unsigned-byte 8)))
           (stream (usocket:socket-stream sock)))
      (setf (socket connection) sock
            (socket-stream connection) stream)))
  connection)
(defmethod connect-to-remote-udp ((connection connection))
  (with-accessors ((ip ip)
                   (port port))
      connection
    (let* ((sock (usocket:socket-connect ip port
                                         :element-type '(unsigned-byte 8)
                                         :protocol :datagram)))
      (setf (socket connection) sock
            (socket-stream connection) :UDP)))
  connection)
      
(defmethod tcp-staggered-and-timed-send ((con connection) list-to-send &key (sleep 0)(stagger 1))
  (with-accessors ((stream socket-stream))
      con
    (loop :for char-code :in list-to-send
          :for x := 0 :then (1+ x)
          :when (zerop (mod x stagger))
            :do (sleep sleep)
          :do (write-byte char-code stream))))


(defun test-send (ip port sleep stagger form)
  (let* ((list (generate-fuzzy-list form))
         (con (connect-to-remote-tcp (make-connection ip port))))
    (tcp-staggered-and-timed-send con list
                                  :stagger stagger
                                  :sleep sleep)))
