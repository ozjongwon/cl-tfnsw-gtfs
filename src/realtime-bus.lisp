(in-package #:realtime-bus)

(defconstant +unix-epoch-difference+
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun decode-unix-time (unix-time)
  (decode-universal-time (unix-to-universal-time unix-time)))

(defun unix-time->string (unix-time)
  (multiple-value-bind (sec min hour)
      (decode-unix-time unix-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

;;;
;;; Low level GTFS stuff
;;;
(defgeneric gtfs->cl-value (val)
  (:documentation "Convert gtfs slot value to Lisp friendly value")
  (:method (val)
    val))

(defgeneric gtfs-kv->value (k v)
  (:documentation "Convert gtfs key value value to keyword")
  (:method (k v)
    (declare (ignore k))
    v))

(defconstant +vehicle-current-status-array+
  (make-array 3 :initial-contents '(:incoming-at :stopped-at :in-transit-to)))

(defmethod gtfs-kv->value ((k (eql :current-status)) v)
  (svref +vehicle-current-status-array+ v))

(defconstant +vehicle-occupancy-status-array+
  (make-array 7 :initial-contents
              '(:empty :many-seats-available :few-seats-available :standing-room-only
                :crushed-standing-room-only :full :not-accepting-passengers)))

(defmethod gtfs-kv->value ((k (eql :occupancy-status)) v)
  (svref +vehicle-occupancy-status-array+ v))

(defconstant +vehicle-congestion-level-array+
  (make-array 5 :initial-contents
              '(:unknown-congestion-level :running-smoothly :stop-and-go :congestion :severe-congestion)))

(defmethod gtfs-kv->value ((k (eql :congestion-level)) v)
  (svref +vehicle-congestion-level-array+ v))

(defconstant +vehicle-schedule-relationship-array+
  (make-array 4 :initial-contents
              '(:scheduled :added :unscheduled :canceled)))

(defmethod gtfs-kv->value ((k (eql :schedule-relationship)) v)
  (svref +vehicle-schedule-relationship-array+ v))

(defmethod gtfs-kv->value  ((k (eql :stop-time-update)) v)
  (map 'list #'gtfs->cl-value v))

(defmethod gtfs-kv->value  ((k (eql :id)) v)
  (loop for k in '(:operator-id :todistripid :todis-contract-id :todis-route-id :trip-instance-number)
     and v in (split-sequence #\_ v)
     unless (zerop (length v))
       collect (cons k v)))
;;
;;
(defun gtfs-message->key-value-pair (gtfs-message keyword)
  (let ((v (gtfs->cl-value (funcall (keyword->reader keyword) gtfs-message))))
    (when v
      (cons keyword (gtfs-kv->value keyword v)))))

(defun gtfs-message->key-value-assoc-list (gtfs-message keywords)
  (loop for k in keywords
     as kv = (gtfs-message->key-value-pair gtfs-message k)
     when kv
     collect kv))

(defmethod gtfs->cl-value ((val pb::%sf%))
  (let ((result-str (pb:string-value val)))
    (unless (zerop (length result-str))
      result-str)))

(defmethod gtfs->cl-value ((val trip-update-stop-time-event))
  (gtfs-message->key-value-assoc-list val
                                    '(:delay :time :uncertainty)))

(defmethod gtfs->cl-value ((val trip-update-stop-time-update))
  (gtfs-message->key-value-assoc-list val
                                    '(:stop-sequence :stop-id :arrival :departure :schedule-relationship)))

(defmethod gtfs->cl-value ((val trip-update))
  (gtfs-message->key-value-assoc-list val
                                    ;; :delay is not in the realtime data
                                    '(:trip :vehicle :stop-time-update :timestamp)))

(defmethod gtfs->cl-value ((val vehicle-descriptor))
  (gtfs-message->key-value-assoc-list val
                                    ;; :id is same as entity's ID
                                    '(:label :license-plate)))

(defmethod gtfs->cl-value ((val position))
  (gtfs-message->key-value-assoc-list val
                                    ;;  :odometer is not in the realtime data
                                    '(:latitude :longitude :bearing :speed)))

(defmethod gtfs->cl-value ((val trip-descriptor))
  (gtfs-message->key-value-assoc-list val
                                    ;; :direction-id is not in the realtime data
                                    '(:trip-id :route-id  :start-time :start-date :schedule-relationship)))

(defmethod gtfs->cl-value ((val vehicle-position))
  (gtfs-message->key-value-assoc-list val
                                    ;;  :current-stop-sequence is not in the realtime data
                                    '(:trip :vehicle :position :stop-id
                                      :current-status :timestamp :congestion-level :occupancy-status)))

(defun keyword->reader (keyword)
  (let ((fn (fdefinition (intern (symbol-name keyword)))))
    (assert fn)
    fn))

(defun header-information (feed-message)
  (gtfs-message->key-value-assoc-list (header feed-message)
                                    '(:gtfs-realtime-version :incrementality :timestamp)))

(defun entity-information (entity)
  (gtfs-message->key-value-assoc-list entity
                                    '(:id :is-deleted :trip-update :vehicle :alert)))

;; Let's start with example file
(defun read-example-realtime-data (filename)
  (let ((feed-message (make-instance 'feed-message)))
    (when (probe-file filename)
      (with-open-file (input filename :direction :input :element-type 'unsigned-byte)
        (let* ((size (file-length input))
               (buffer (make-array size :element-type '(unsigned-byte 8))))
          (read-sequence buffer input)
          (pb:merge-from-array feed-message buffer 0 size))))
    feed-message))
