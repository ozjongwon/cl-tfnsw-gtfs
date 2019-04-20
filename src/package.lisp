;;;;    package.lisp

(in-package #:common-lisp-user)

(defpackage #:realtime-bus
  (:use #:common-lisp #:split-sequence)
  (:import-from #:transit-realtime
                #:feed-message
                #:feed-header
                #:feed-entity
                #:header
                #:gtfs-realtime-version
                #:incrementality
                #:timestamp

                #:entity
                #:id
                #:is-deleted
                #:trip-update
                #:vehicle
                #:alert

                #:trip
                #:current-stop-sequence
                #:stop-id

                #:current-status
                #:congestion-level
                #:occupancy-status

                #:stop-time-update
                #:delay

                #:trip-descriptor
                #:trip-id
                #:route-id
                #:direction-id
                #:start-time
                #:start-date
                #:schedule-relationship

                #:vehicle-position
                #:vehicle-descriptor
                #:latitude
                #:longitude
                #:bearing
                #:odometer
;;                #:speed
                #:label
                #:license-plate

                #:trip-update-stop-time-update
                #:stop-sequence
                #:arrival
                #:departure

                #:trip-update-stop-time-event
                #:delay
                :uncertainty

                )
  ;; The "number" and "type" field names conflict with Common Lisp symbols.
  (:shadowing-import-from #:transit-realtime
                          #:position
                          #:time)
  ;; (:export #:add-person
  ;;          #:list-people)
  )
