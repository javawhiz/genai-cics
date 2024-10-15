             CBL CICS('COBOL3') APOST
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DFH0XECC.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'DFH0XECC------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-CALEN                 PIC S9(4) COMP.

       01  ABS-TIME                    PIC S9(8) COMP VALUE +0.
       01  TIME1                       PIC X(8)  VALUE SPACES.
       01  DATE1                       PIC X(10) VALUE SPACES.

       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' DFH0XECC'.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-DETAIL                PIC X(50) VALUE SPACES.

       01 WORKING-VARIABLES.
           03 WS-REQUEST-ID            PIC X(6).
           03 WS-RESP                  PIC S9(8) COMP.
           03 WS-RESP2                 PIC S9(8) COMP.
           03 WS-OPERATION             PIC X(255).
           03 WS-SERVICE-CONT-NAME     PIC X(16).
           03 WS-CHANNELNAME           PIC X(16).
           03 WS-ENDPOINT-URI          PIC X(255).

       01 INQUIRE-CATALOG-REQUEST.
           COPY DFH0XCPA.
       01 INQUIRE-CATALOG-RESPONSE.
           COPY DFH0XCPB.
       01 INQUIRE-SINGLE-REQUEST.
           COPY DFH0XCPC.
       01 INQUIRE-SINGLE-RESPONSE.
           COPY DFH0XCPD.
       01 PLACE-ORDER-REQUEST.
           COPY DFH0XCPE.
       01 PLACE-ORDER-RESPONSE.
           COPY DFH0XCPF.

       01 EXAMPLE-APP-CONFIG       PIC X(9)
               VALUE 'WS-SERVER'.

       01 APP-CONFIG.
           03 FILE-KEY             PIC X(9).
           03 FILLER               PIC X.
           03 SERVER-LOCATION      PIC X(200).

       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY DFH0XCP1.


       PROCEDURE DIVISION.


       MAINLINE SECTION.


           INITIALIZE APP-CONFIG
           INITIALIZE ERROR-MSG
           INITIALIZE INQUIRE-CATALOG-REQUEST
           INITIALIZE INQUIRE-CATALOG-RESPONSE
           INITIALIZE INQUIRE-SINGLE-REQUEST
           INITIALIZE INQUIRE-SINGLE-RESPONSE
           INITIALIZE PLACE-ORDER-REQUEST
           INITIALIZE PLACE-ORDER-RESPONSE
           INITIALIZE WORKING-VARIABLES


           MOVE EIBTRNID TO WS-TRANSID
           MOVE EIBTRMID TO WS-TERMID
           MOVE EIBTASKN TO WS-TASKNUM


           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-DETAIL
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('EXCA') NODUMP END-EXEC
           END-IF


           MOVE '00' TO CA-RETURN-CODE IN DFHCOMMAREA
           MOVE EIBCALEN TO WS-CALEN.


           EXEC CICS READ FILE('EXMPCONF')
                          INTO(APP-CONFIG)
                          RIDFLD(EXAMPLE-APP-CONFIG)
                          RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT EQUAL DFHRESP(NORMAL)
               MOVE '51' TO CA-RETURN-CODE IN DFHCOMMAREA
               MOVE 'APPLICATION ERROR OPENING CONFIGURATION FILE'
                   TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
               EXEC CICS RETURN END-EXEC
           END-IF




           MOVE 'DFHWS-DATA' TO WS-SERVICE-CONT-NAME


           MOVE 'SERVICE-CHANNEL' TO WS-CHANNELNAME


           MOVE FUNCTION UPPER-CASE(CA-REQUEST-ID IN DFHCOMMAREA)
                           TO WS-REQUEST-ID

           EVALUATE WS-REQUEST-ID
               WHEN '01INQC'

                   PERFORM CATALOG-INQUIRE

               WHEN '01INQS'

                   PERFORM SINGLE-INQUIRE

               WHEN '01ORDR'

                   PERFORM PLACE-ORDER

               WHEN OTHER

                   PERFORM REQUEST-NOT-RECOGNISED

           END-EVALUATE


           EXEC CICS RETURN END-EXEC.

       MAINLINE-EXIT.
           EXIT.



       WRITE-ERROR-MESSAGE.

           EXEC CICS ASKTIME ABSTIME(ABS-TIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(ABS-TIME)
                     MMDDYYYY(DATE1)
                     TIME(TIME1)
           END-EXEC
           MOVE DATE1 TO EM-DATE
           MOVE TIME1 TO EM-TIME

           EXEC CICS WRITEQ TD QUEUE('CSMT')
                     FROM(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
           EXIT.


        CATALOG-INQUIRE.


           MOVE CA-REQUEST-ID IN DFHCOMMAREA
               TO caXrequestXid IN INQUIRE-CATALOG-REQUEST
           MOVE 00 TO caXreturnXcode IN INQUIRE-CATALOG-REQUEST
           MOVE CA-LIST-START-REF IN DFHCOMMAREA
               TO caXlistXstartXref IN INQUIRE-CATALOG-REQUEST


           EXEC CICS PUT CONTAINER(WS-SERVICE-CONT-NAME)
                         CHANNEL(WS-CHANNELNAME)
                         FROM(INQUIRE-CATALOG-REQUEST)
           END-EXEC



           STRING  'http://'
                   SERVER-LOCATION
                   '/exampleApp/inquireCatalog'
               DELIMITED BY SPACE
               INTO WS-ENDPOINT-URI
           END-STRING






           MOVE 'DFH0XCMNOperation' TO WS-OPERATION

           EXEC CICS INVOKE WEBSERVICE('inquireCatalogClient')
                     CHANNEL(WS-CHANNELNAME)
                     URI(WS-ENDPOINT-URI)
                     OPERATION(WS-OPERATION)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC.



           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)



                   EXEC CICS GET CONTAINER(WS-SERVICE-CONT-NAME)
                             CHANNEL(WS-CHANNELNAME)
                             INTO(INQUIRE-CATALOG-RESPONSE)
                   END-EXEC

                   MOVE caXreturnXcode IN INQUIRE-CATALOG-RESPONSE
                        TO CA-RETURN-CODE IN DFHCOMMAREA

                   MOVE caXresponseXmessage IN INQUIRE-CATALOG-RESPONSE
                        TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA


                   IF caXreturnXcode IN INQUIRE-CATALOG-RESPONSE
                     EQUAL 00

                       MOVE caXinquireXrequest
                               IN INQUIRE-CATALOG-RESPONSE
                       TO   CA-INQUIRE-REQUEST IN DFHCOMMAREA
                   END-IF

               WHEN DFHRESP(INVREQ)
                   MOVE
                   'Error calling inquire catalog service - INVREQ'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 30 TO CA-RETURN-CODE IN DFHCOMMAREA

               WHEN DFHRESP(NOTFND)
                   MOVE
                   'Error calling inquire catalog service - NOT FOUND'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 31 TO CA-RETURN-CODE IN DFHCOMMAREA

               WHEN OTHER
                   MOVE
                   'Error calling inquire catalog service'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 32 TO CA-RETURN-CODE IN DFHCOMMAREA
           END-EVALUATE.

           EXIT.





        SINGLE-INQUIRE.



           MOVE CA-REQUEST-ID IN DFHCOMMAREA
               TO caXrequestXid IN INQUIRE-SINGLE-REQUEST
           MOVE 00 TO caXreturnXcode IN INQUIRE-SINGLE-REQUEST
           MOVE CA-ITEM-REF-REQ IN DFHCOMMAREA
               TO caXitemXrefXreq IN INQUIRE-SINGLE-REQUEST






           EXEC CICS PUT CONTAINER(WS-SERVICE-CONT-NAME)
                         CHANNEL(WS-CHANNELNAME)
                         FROM(INQUIRE-SINGLE-REQUEST)
           END-EXEC






           STRING  'http://'
                   SERVER-LOCATION
                   '/exampleApp/inquireSingle'
               DELIMITED BY SPACE
               INTO WS-ENDPOINT-URI
           END-STRING






           MOVE 'DFH0XCMNOperation' TO WS-OPERATION

           EXEC CICS INVOKE WEBSERVICE('inquireSingleClient')
                     CHANNEL(WS-CHANNELNAME)
                     URI(WS-ENDPOINT-URI)
                     OPERATION(WS-OPERATION)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC.



           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)



                   EXEC CICS GET CONTAINER(WS-SERVICE-CONT-NAME)
                             CHANNEL(WS-CHANNELNAME)
                             INTO(INQUIRE-SINGLE-RESPONSE)
                   END-EXEC

                   MOVE caXreturnXcode IN INQUIRE-SINGLE-RESPONSE
                        TO CA-RETURN-CODE IN DFHCOMMAREA

                   MOVE caXresponseXmessage IN INQUIRE-SINGLE-RESPONSE
                        TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA


                   IF caXreturnXcode IN INQUIRE-SINGLE-RESPONSE
                     EQUAL 00

                       MOVE caXsingleXitem
                               IN INQUIRE-SINGLE-RESPONSE
                       TO   CA-SINGLE-ITEM IN DFHCOMMAREA
                   END-IF

               WHEN DFHRESP(INVREQ)
                   MOVE
                   'Error calling inquire single service - INVREQ'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 30 TO CA-RETURN-CODE IN DFHCOMMAREA

               WHEN DFHRESP(NOTFND)
                   MOVE
                   'Error calling inquire single service - NOT FOUND'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 31 TO CA-RETURN-CODE IN DFHCOMMAREA

               WHEN OTHER
                   MOVE
                   'Error calling inquire single service'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 32 TO CA-RETURN-CODE IN DFHCOMMAREA
           END-EVALUATE.

           EXIT.





        PLACE-ORDER.


           MOVE CA-REQUEST-ID IN DFHCOMMAREA
               TO caXrequestXid IN PLACE-ORDER-REQUEST
           MOVE 00 TO caXreturnXcode IN PLACE-ORDER-REQUEST
           MOVE CA-USERID IN DFHCOMMAREA
               TO caXuserid IN PLACE-ORDER-REQUEST
           MOVE CA-CHARGE-DEPT IN DFHCOMMAREA
               TO caXchargeXdept IN PLACE-ORDER-REQUEST
           MOVE CA-ITEM-REF-NUMBER IN DFHCOMMAREA
               TO caXitemXrefXnumber IN PLACE-ORDER-REQUEST
           MOVE CA-QUANTITY-REQ IN DFHCOMMAREA
               TO caXquantityXreq IN PLACE-ORDER-REQUEST







           EXEC CICS PUT CONTAINER(WS-SERVICE-CONT-NAME)
                         CHANNEL(WS-CHANNELNAME)
                         FROM(PLACE-ORDER-REQUEST)
           END-EXEC






           STRING  'http://'
                   SERVER-LOCATION
                   '/exampleApp/placeOrder'
               DELIMITED BY SPACE
               INTO WS-ENDPOINT-URI
           END-STRING






           MOVE 'DFH0XCMNOperation' TO WS-OPERATION

           EXEC CICS INVOKE WEBSERVICE('placeOrderClient')
                     CHANNEL(WS-CHANNELNAME)
                     URI(WS-ENDPOINT-URI)
                     OPERATION(WS-OPERATION)
                     RESP(WS-RESP) RESP2(WS-RESP2)
           END-EXEC.



           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)



                   EXEC CICS GET CONTAINER(WS-SERVICE-CONT-NAME)
                             CHANNEL(WS-CHANNELNAME)
                             INTO(PLACE-ORDER-RESPONSE)
                   END-EXEC

                   MOVE caXreturnXcode IN PLACE-ORDER-RESPONSE
                        TO CA-RETURN-CODE IN DFHCOMMAREA

                   MOVE caXresponseXmessage IN PLACE-ORDER-RESPONSE
                        TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA


                   IF caXreturnXcode IN PLACE-ORDER-RESPONSE
                     EQUAL 00

                       MOVE caXorderXrequest
                               IN PLACE-ORDER-RESPONSE
                       TO   CA-ORDER-REQUEST IN DFHCOMMAREA
                   END-IF

               WHEN DFHRESP(INVREQ)
                   MOVE
                   'Error calling place order service - INVREQ'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 30 TO CA-RETURN-CODE IN DFHCOMMAREA

               WHEN DFHRESP(NOTFND)
                   MOVE
                   'Error calling place order service - NOT FOUND'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 31 TO CA-RETURN-CODE IN DFHCOMMAREA

               WHEN OTHER
                   MOVE
                   'Error calling place order service'
                       TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
                   MOVE 32 TO CA-RETURN-CODE IN DFHCOMMAREA
           END-EVALUATE.

           EXIT.





        REQUEST-NOT-RECOGNISED.
           MOVE '99' TO CA-RETURN-CODE IN DFHCOMMAREA

           STRING ' UNKNOWN REQUEST ID RECEIVED - '
               CA-REQUEST-ID IN DFHCOMMAREA
               DELIMITED BY SIZE
               INTO EM-DETAIL
           END-STRING

           MOVE 'OPERATION UNKNOWN'
               TO CA-RESPONSE-MESSAGE IN DFHCOMMAREA
           EXIT.
