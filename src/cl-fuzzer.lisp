;;;; cl-fuzzer.lisp
(in-package #:cl-fuzzer)
(defclass connection ()
  ((ip
    :accessor ip
    :initarg :ip
    :documentation "Slot containing the IP to connect to")
   (port
    :accessor port
    :initarg :port
    :documentation "slot containing the port number to connect to")
   (socket
    :accessor socket
    :documentation "slot to contain the socket made using usocket")
   (socket-stream
    :accessor socket-stream
    :documentation "the socket stream for reading/writing")
   (received
    :accessor received
    :initform nil
    :documentation "what is received from the server"))
  (:documentation "Used to connect to a connection you wish to attempt to fuzz"))

(defmethod print-object ((object connection) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "IP: ~S~%PORT: ~S~%RECEIVED: ~S~%"
            (ip object)
            (port object)
            (if (null (received object))
                :RECEIVED-NOTHING-WHEN-FUZZING
                (received object)))))

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
    (handler-case 
        (loop :for char-code :in list-to-send
              :for x := 0 :then (1+ x)
              :do (read-all-from-stream-if-can con)
              :when (zerop (mod x stagger))
                :do (sleep sleep)
              :do (write-byte char-code stream)
                  (force-output stream))
      (SB-INT:SIMPLE-STREAM-ERROR ()))))

(defmethod readablep ((con connection))
  (listen (socket-stream con)))

(defmethod read-all-from-stream-if-can ((con connection))
  "reads all from the stream if there is anything to read, if not just returns nil. if it can read,
it'll read all it can from the stream and set (received con) to that"
  (with-accessors ((stream socket-stream)
                   (received received))
      con
    (let ((bytes
            (loop :for can-read? := (readablep con) :then (readablep con)
                  :if can-read?
                    :collect (read-byte stream)
                  :else
                    :do (return nil))))
      
      (if (listp bytes)
          (setf received (append received
                                 (mapcar #'code-char bytes)))
          nil))))


(defun fuzz (ip port sleep stagger form)
  (let* ((list (generate-fuzzy-list form))
         (con (connect-to-remote-tcp (make-connection ip port))))
    (print list)
    (sleep 1)
    (tcp-staggered-and-timed-send con list
                                  :stagger stagger
                                  :sleep sleep)
    (sleep 1)
    (read-all-from-stream-if-can con)
    con))
