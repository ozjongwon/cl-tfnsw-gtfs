# Introduction

I have an idea  (which is relevant to traffic data) to commercialise about a week ago.
Then my friends pointed there are such services already.
I can still implement my idea, but all fun parts (like building embedded systems to collect live data,
implementing Kalman filter, building predictive models, etc) are gone now.



A couple of days ago, I started to look at TfNSW realtime data and implemented reading 'Google protocol buffers'
formatted data. The implementation is about 150 lines of Lisp code, and even though it is written in one of
the most powerful programming languages and may challenge to do similar style of programming in other programming
languages, the approach I took may still help other programmers. So I decided to make it MIT licensed
open source project.


# General steps to build 'protocol buffers' based applications
The first step is defining .proto files - for TfNSW realtime data, I copied a .proto
file(saved as tfnsw-gtfs-realtime.proto, which has some minimum changes).


Then compile the '.proto' file using a compiler to your choice of programming language -
in my case I use Common Lisp implementation of protobuf compiler -  https://common-lisp.net/project/protobuf/


After compilation, I got tfnsw-gtfs-realtime.lisp file, then I can read or write protocol buffers data from Common Lisp world.


# A bit of tweak

When I played with the compiler, I found the generated code was not what I expected, so I forked the original
repo and made some changes(https://github.com/ozjongwon/protobuf).

Basically, my changes don't create empty protocol buffers data and leave as NIL
(which is null value in Common Lisp)

# The design and implementation
Common Lisp is a powerful programming language, and the protocol buffers compiler generated Lisp code
is easy to handle using this power.

What I have done is basically from each message structure, get all field values from field names and
build a nested structure(which is easy to manipulate using Lisp's "list" operations).

I guess there can be three different approaches from static to dynamic.

I bet a typical programming language users type everything manually, which is very tedious and error-prone.
Using the dynamic feature of Lisp, I could ask to each 'protocol buffers record' about its field names
and values then build Lisp friendly structure, which will have a bit of runtime overhead but future proof.

The approach I've taken is middle of the two (but closer to dynamic side). I listed field names
and at runtime proper functions get called to get values and build a list structure.

# How to try it?
If you're not familiar with Common Lisp environment, it can be challenging.
I can tell an instance of general procedures:

1. Choose a development platform. I use Ubuntu Linux -https://www.ubuntu.com/download/desktop
2. Choose a Common Lisp implementation. I use Clozure Common Lisp - https://ccl.clozure.com/download.html
3. Install Quick Lisp - https://www.quicklisp.org/beta/
4. Clone Common Lisp implementation of protocol buffers compiler - https://github.com/ozjongwon/protobuf
5. Follow instruction of protobuf
6. Start Clozure Common Lisp REPL(Read-Eval-Print-Loop)
7. Follow 'An example session'


# An example session
To write this code, I downloaded two files - a data file(full_greater_sydney_gtfs_static.zip)
and a specification file(TfNSW_GTFS_Realtime_Buses_Technical_Doc.pdf)

## Download an example realtime data
I used bus data.

```
curl -X GET --header 'Accept: application/x-google-protobuf' --header 'Authorization: apikey <Your API Key>' 'https://api.transport.nsw.gov.au/v1/gtfs/realtime/buses' --output /tmp/tfnsw-gtfs-realtime.ex
```

## Load cl-tfnsw-gtfs system
On Lisp REPL, load system definition file, then load the system.

```
CL-USER> (load "<path to the local cl-tfnsw-gtfs repo>/cl-tfnsw-gtfs.asd")
#P"<path to the local cl-tfnsw-gtfs repo>/cl-tfnsw-gtfs/cl-tfnsw-gtfs.asd"

CL-USER> (ql:quickload "cl-tfnsw-gtfs")
To load "cl-tfnsw-gtfs":
  Load 1 ASDF system:
    cl-tfnsw-gtfs
; Loading "cl-tfnsw-gtfs"
..................................................
[package realtime-bus].
("cl-tfnsw-gtfs")
```

## Change package
On Lisp REPL, change the namespace to realtime-bus.

```
CL-USER> (in-package #:realtime-bus)
#<Package "REALTIME-BUS">
REALTIME-BUS>
```

## Read the example realtime data and assign the result to a variable

Using 'read-example-realtime-data' function, you can read the realtime data into Lisp.
In real applications, you can use the response of a HTTP request to a realtime data endpoint
to read the data into Lisp.

```
(progn
  (setq *realtime-ex* (read-example-realtime-data "/tmp/tfnsw-gtfs-realtime.ex"))
   nil)
```

## Check the type of the record
The type of variable is 'FEED-MESSAGE', which is from 'message FeedMessage' in the proto definition file.
```
REALTIME-BUS> (type-of *realtime-ex*)
FEED-MESSAGE
```

## Check the header using header-infomation function
```
REALTIME-BUS> (header-information  *realtime-ex*)
((:GTFS-REALTIME-VERSION . "1.0") (:INCREMENTALITY . 0) (:TIMESTAMP . 1555663281))
```

## Check the entity using entity function
The function 'entity' is from field name 'entity' of 'message FeedMessage' in the proto definition file.
It shows the low level protocol buffers records.

```
REALTIME-BUS> (type-of (entity *realtime-ex*))
(VECTOR T 2048)
REALTIME-BUS> (aref (entity *realtime-ex*) 0)
#<FEED-ENTITY  id: #<%SF% "8266___1B_1"> trip-update: #<TRIP-UPDATE  trip: #<TRIP-DESCRIPTOR  route-id: #<%SF% "_1B"> start-time: #<%SF% "18:37:04"> start-date: #<%SF% "20190419"> schedule-relationship: 2 #x302000E4699D> vehicle: #<VEHICLE-DESCRIPTOR  id: #<%SF% "8266___1B_1"> #x302000E44BED> stop-time-update: #(#<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 2 stop-id: #<%SF% "213845"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663611 #x302000E4678D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663621 #x302000E4672D> schedule-relationship: 0 #x302000E467ED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 3 stop-id: #<%SF% "211269"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663695 #x302000E4660D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663708 #x302000E465AD> schedule-relationship: 0 #x302000E4666D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 4 stop-id: #<%SF% "2112247"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663821 #x302000E4648D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663849 #x302000E4642D> schedule-relationship: 0 #x302000E464ED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 5 stop-id: #<%SF% "2112234"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663935 #x302000E4630D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555663959 #x302000E462AD> schedule-relationship: 0 #x302000E4636D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 6 stop-id: #<%SF% "2113166"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664072 #x302000E4618D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664078 #x302000E4612D> schedule-relationship: 0 #x302000E461ED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 7 stop-id: #<%SF% "2113207"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664268 #x302000E4600D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664284 #x302000E45FAD> schedule-relationship: 0 #x302000E4606D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 8 stop-id: #<%SF% "2073135"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664504 #x302000E45E8D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664522 #x302000E45E2D> schedule-relationship: 0 #x302000E45EED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 9 stop-id: #<%SF% "207319"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664727 #x302000E45D0D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664741 #x302000E45CAD> schedule-relationship: 0 #x302000E45D6D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 10 stop-id: #<%SF% "207544"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664850 #x302000E45B8D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664876 #x302000E45B2D> schedule-relationship: 0 #x302000E45BED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 11 stop-id: #<%SF% "2075114"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664918 #x302000E45A0D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555664932 #x302000E459AD> schedule-relationship: 0 #x302000E45A6D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 12 stop-id: #<%SF% "2075120"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665003 #x302000E4588D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665018 #x302000E4582D> schedule-relationship: 0 #x302000E458ED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 13 stop-id: #<%SF% "207548"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665078 #x302000E4570D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665087 #x302000E456AD> schedule-relationship: 0 #x302000E4576D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 14 stop-id: #<%SF% "208450"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665522 #x302000E4558D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665545 #x302000E4552D> schedule-relationship: 0 #x302000E455ED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 15 stop-id: #<%SF% "210163"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665796 #x302000E4540D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555665803 #x302000E453AD> schedule-relationship: 0 #x302000E4546D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 16 stop-id: #<%SF% "210339"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666188 #x302000E4528D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666215 #x302000E4522D> schedule-relationship: 0 #x302000E452ED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 17 stop-id: #<%SF% "210378"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666259 #x302000E4510D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666270 #x302000E450AD> schedule-relationship: 0 #x302000E4516D> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 18 stop-id: #<%SF% "210380"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666299 #x302000E44F8D> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666315 #x302000E44F2D> schedule-relationship: 0 #x302000E44FED> #<TRIP-UPDATE-STOP-TIME-UPDATE  stop-sequence: 19 stop-id: #<%SF% "210260"> arrival: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666379 #x302000E44CFD> departure: #<TRIP-UPDATE-STOP-TIME-EVENT  delay: 0 time: 1555666388 #x302000E44C9D> schedule-relationship: 0 #x302000E44D5D>) timestamp: 1555663269 #x302000E46A3D> #x302000E46B0D>
REALTIME-BUS>
```

## Check the entity using entity-information function
Parse the low-level representation data to Lisp friendly data using entity-information.
You can also parse every entity using 'map' function - the amount of the output will be huge, so let's do that
with the first 10 entities.

```
REALTIME-BUS> (entity-information (aref (entity *realtime-ex*) 0))
((:ID (:OPERATOR-ID . "8266") (:TODIS-ROUTE-ID . "1B") (:TRIP-INSTANCE-NUMBER . "1")) (:TRIP-UPDATE (:TRIP (:ROUTE-ID . "_1B") (:START-TIME . "18:37:04") (:START-DATE . "20190419") (:SCHEDULE-RELATIONSHIP . :UNSCHEDULED)) (:STOP-TIME-UPDATE ((:STOP-SEQUENCE . 2) (:STOP-ID . "213845") (:ARRIVAL (:DELAY . 0) (:TIME . 1555663611) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555663621) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 3) (:STOP-ID . "211269") (:ARRIVAL (:DELAY . 0) (:TIME . 1555663695) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555663708) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 4) (:STOP-ID . "2112247") (:ARRIVAL (:DELAY . 0) (:TIME . 1555663821) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555663849) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 5) (:STOP-ID . "2112234") (:ARRIVAL (:DELAY . 0) (:TIME . 1555663935) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555663959) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 6) (:STOP-ID . "2113166") (:ARRIVAL (:DELAY . 0) (:TIME . 1555664072) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555664078) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 7) (:STOP-ID . "2113207") (:ARRIVAL (:DELAY . 0) (:TIME . 1555664268) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555664284) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 8) (:STOP-ID . "2073135") (:ARRIVAL (:DELAY . 0) (:TIME . 1555664504) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555664522) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 9) (:STOP-ID . "207319") (:ARRIVAL (:DELAY . 0) (:TIME . 1555664727) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555664741) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 10) (:STOP-ID . "207544") (:ARRIVAL (:DELAY . 0) (:TIME . 1555664850) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555664876) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 11) (:STOP-ID . "2075114") (:ARRIVAL (:DELAY . 0) (:TIME . 1555664918) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555664932) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 12) (:STOP-ID . "2075120") (:ARRIVAL (:DELAY . 0) (:TIME . 1555665003) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555665018) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 13) (:STOP-ID . "207548") (:ARRIVAL (:DELAY . 0) (:TIME . 1555665078) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555665087) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 14) (:STOP-ID . "208450") (:ARRIVAL (:DELAY . 0) (:TIME . 1555665522) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555665545) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 15) (:STOP-ID . "210163") (:ARRIVAL (:DELAY . 0) (:TIME . 1555665796) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555665803) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 16) (:STOP-ID . "210339") (:ARRIVAL (:DELAY . 0) (:TIME . 1555666188) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555666215) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 17) (:STOP-ID . "210378") (:ARRIVAL (:DELAY . 0) (:TIME . 1555666259) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555666270) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 18) (:STOP-ID . "210380") (:ARRIVAL (:DELAY . 0) (:TIME . 1555666299) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555666315) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED)) ((:STOP-SEQUENCE . 19) (:STOP-ID . "210260") (:ARRIVAL (:DELAY . 0) (:TIME . 1555666379) (:UNCERTAINTY . 0)) (:DEPARTURE (:DELAY . 0) (:TIME . 1555666388) (:UNCERTAINTY . 0)) (:SCHEDULE-RELATIONSHIP . :SCHEDULED))) (:TIMESTAMP . 1555663269)))
REALTIME-BUS>
REALTIME-BUS> (map 'list #'entity-information (subseq (entity *realtime-ex*) 0 10))
;; A long list of the output

```

