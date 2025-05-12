CLASS zcl_student_um DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES :
      tt_create_student TYPE TABLE FOR CREATE zistudent_um\\student,
      tt_mapped         TYPE RESPONSE FOR MAPPED EARLY zistudent_um,
      tt_response       TYPE RESPONSE FOR FAILED EARLY zistudent_um,
      tt_reported       TYPE RESPONSE FOR REPORTED EARLY zistudent_um,
      tt_reported_late  TYPE RESPONSE FOR REPORTED LATE zistudent_um,
      tt_student_keys   TYPE TABLE FOR READ IMPORT zistudent_um\\student,
      tt_student_update TYPE TABLE FOR UPDATE zistudent_um\\student,
      tt_results_update type table for update zistudent_um\\results,
      tt_student_delete TYPE TABLE FOR DELETE zistudent_um\\student,
      tt_create_results TYPE TABLE FOR CREATE zistudent_um\\student\_results,
      tt_student_keys1 type table for read import zistudent_um\\student\_results,
      tt_read_link type table for read link zistudent_um\\student\_results,
     tt_student_result1 type table for read result zistudent_um\\student\_results,
     tt_result_keys type table for read import zistudent_um\\results,
     tt_result_delete type table for delete zistudent_um\\results,
     tt_result type table for read result zistudent_um\\results,
      result_r    type c length 1,
      tt_student_result TYPE TABLE FOR READ RESULT zistudent_um\\student.

    "create constructor
    CLASS-METHODS : get_Instance RETURNING VALUE(ro_instance) TYPE REF TO zcl_student_um.

    DATA: gt_student  TYPE STANDARD TABLE OF zmstudent_um,
          gt_result type STANDARD TABLE OF zmacademic_um.

    METHODS :
      earlynumbering_create
        IMPORTING entities TYPE  tt_create_student "table for create zistudent_um\\student
        CHANGING  mapped   TYPE tt_mapped "response for mapped early zistudent_um
                  failed   TYPE tt_response "response for failed early zistudent_um
                  reported TYPE tt_reported ,"response for reported early zistudent_um

      create_student
        IMPORTING entities TYPE tt_create_student "table for create zistudent_um\\student
        CHANGING  mapped   TYPE tt_mapped "response for mapped early zistudent_um
                  failed   TYPE tt_response "response for failed early zistudent_um
                  reported TYPE tt_reported,

      read_student
        IMPORTING keys     TYPE tt_student_keys "table for read import zistudent_um\\student
        CHANGING  result   TYPE tt_student_result "table for read result zistudent_um\\student
                  failed   TYPE tt_response "response for failed early zistudent_um
                  reported TYPE tt_reported, "response for reported early zistudent_um

       read_results
        importing   keys    type tt_result_keys"table for read import zistudent_um\\results    [ derived type... ]
        changing    result  type tt_result "table for read result zistudent_um\\results    [ derived type... ]
                    failed  type tt_response"response for failed early zistudent_um [ derived type... ]
                    reported    type tt_reported,"response for reported early zistudent_um

      update_student
        IMPORTING entities TYPE tt_student_update  "table for update zistudent_um\\student [ derived type... ]
        CHANGING  mapped   TYPE tt_mapped "response for mapped early zistudent_um [ derived type... ]
                  failed   TYPE tt_response "response for failed early zistudent_um [ derived type... ]
                  reported TYPE tt_reported, "response for reported early zistudent_um   [ derived type... ]

      update_results
        importing   entities  type tt_results_update "table for update zistudent_um\\results [ derived type... ]
        changing    mapped  type tt_mapped"response for mapped early zistudent_um [ derived type... ]
                    failed  type tt_response"response for failed early zistudent_um [ derived type... ]
                    reported    type tt_reported,"response for reported early zistudent_um   [ derived type... ]

      delete_student
        IMPORTING keys     TYPE tt_student_delete "table for delete zistudent_um\\student [ derived type... ]
        CHANGING  mapped   TYPE tt_mapped "response for mapped early zistudent_um [ derived type... ]
                  failed   TYPE tt_response "response for failed early zistudent_um [ derived type... ]
                  reported TYPE tt_reported ,"response for reported early zistudent_um   [ derived type... ]

      earlynumbering_create_results
        IMPORTING entities TYPE tt_create_results "table for create zistudent_um\\student\_results    [ derived type... ]
        CHANGING  mapped   TYPE tt_mapped "response for mapped early zistudent_um [ derived type... ]
                  failed   TYPE tt_response "response for failed early zistudent_um [ derived type... ]
                  reported TYPE tt_reported, "response for reported early zistudent_um   [ derived type... ]

      cba_results
        IMPORTING entities_cba TYPE tt_create_results "table for create zistudent_um\\student\_results    [ derived type... ]
        CHANGING  mapped       TYPE tt_mapped"response for mapped early zistudent_um [ derived type... ]
                  failed       TYPE tt_response"response for failed early zistudent_um [ derived type... ]
                  reported     TYPE tt_reported,"response for reported early zistudent_um   [ derived type... ]

      rba_results
        importing   keys_rba    type tt_student_keys1 "table for read import zistudent_um\\student\_results   [ derived type... ]
                    result_requested    type result_r"c length 1
        changing    result  type tt_student_result1 "table for read result zistudent_um\\student\_results   [ derived type... ]
                    association_links   type tt_read_link "table for read link zistudent_um\\student\_results [ derived type... ]
                    failed  type tt_response"response for failed early zistudent_um [ derived type... ]
                    reported    type tt_reported, "response for reported early zistudent_um   [ derived type... ]


      delete_results
        importing   keys    type tt_result_delete "table for delete zistudent_um\\results [ derived type... ]
        changing    mapped  type tt_mapped "response for mapped early zistudent_um [ derived type... ]
                    failed  type tt_response "response for failed early zistudent_um [ derived type... ]
                    reported    type tt_reported,"response for reported early zistudent_um   [ derived type... ]

      get_next_id
        RETURNING
          VALUE(rv_id) TYPE sysuuid_x16 ,

      get_next_student_id
        RETURNING
          VALUE(rv_studentid) TYPE int1,

      saveData
        CHANGING reported TYPE tt_reported_late. "response for reported late zistudent_um




  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA : mo_instance  TYPE REF TO zcl_student_um,
                 "gt_student  TYPE STANDARD TABLE OF zmstudent_um,
                 gs_mapped    TYPE tt_mapped,
                 gr_student_d TYPE RANGE OF zmstudent_um-id,
                 gr_results_d type range of zmacademic_um-id,
                 gr_results_d_course type range of zmacademic_um-course,
                 gr_results_d_semester type range of zmacademic_um-semester,
                 gt_results   TYPE STANDARD TABLE OF zmacademic_um.



ENDCLASS.



CLASS zcl_student_um IMPLEMENTATION.
  METHOD get_instance.

    mo_instance = ro_instance = COND #( WHEN mo_instance IS BOUND
                                        THEN mo_instance
                                        ELSE NEW #( ) ).

  ENDMETHOD.

  METHOD earlynumbering_create.

    DATA(ls_mapped) =  gs_mapped.
    "data(lv_new_id) = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).

    DATA(lv_new_id) = get_next_id( ).

    "Buffer table update

    READ TABLE gt_student ASSIGNING FIELD-SYMBOL(<lfs_student>) INDEX 1.
    IF <lfs_student> IS ASSIGNED.
      <lfs_student>-id = lv_new_id.
      UNASSIGN <lfs_student>.
    ENDIF.

    mapped-student = VALUE #(
    FOR ls_entities IN entities WHERE ( id IS INITIAL )
    (
    %cid = ls_entities-%cid
    %is_draft = ls_entities-%is_draft
    Id = lv_new_id
    )
     ).



  ENDMETHOD.


  METHOD create_student.

    gt_student = CORRESPONDING #( entities MAPPING FROM ENTITY ).

    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      IF NOT gt_student[] IS INITIAL.
        gt_student[ 1 ]-studentid  = get_next_student_id( ).
        mapped-student = VALUE #(  (
                            %cid = <lfs_entities>-%cid
                            %key = <lfs_entities>-%key
                            %is_draft = <lfs_entities>-%is_draft
        ) ).

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_next_id.

    TRY.
        rv_id = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
      CATCH cx_uuid_error.
        "handle exceptions
    ENDTRY.

  ENDMETHOD.

  METHOD get_next_student_id.

    SELECT MAX( studentid ) FROM zmstudent_um INTO @DATA(lv_max_studentid).
    rv_studentid = lv_max_studentid + 1.
  ENDMETHOD.

  METHOD savedata.
    IF NOT gt_student[] IS INITIAL.

      MODIFY zmstudent_um FROM TABLE @gt_student.


    ENDIF.

    IF NOT gt_results[] IS INITIAL.

      MODIFY zmacademic_um FROM TABLE @gt_results.

    ENDIF.


    IF NOT gr_student_d IS INITIAL.

      DELETE FROM zmstudent_um WHERE id IN @gr_student_d.

    ENDIF.


    IF NOT gr_results_d IS INITIAL.

      DELETE FROM zmacademic_um WHERE id IN @gr_results_d
                                       and course in @gr_results_d_course
                                       and semester in @gr_results_d_semester.

    ENDIF.

  ENDMETHOD.

  METHOD read_student.

    SELECT * FROM zmstudent_um FOR ALL ENTRIES IN @keys
    WHERE id = @keys-Id
    INTO TABLE @DATA(lt_student_data).

    result = CORRESPONDING #( lt_student_data MAPPING TO ENTITY ).

  ENDMETHOD.

  METHOD update_student.

    DATA:lt_student_update   TYPE STANDARD TABLE OF zmstudent_um,
         lt_student_update_x TYPE STANDARD TABLE OF zmstudent_cs.

    lt_student_update = CORRESPONDING #( entities MAPPING FROM ENTITY ).
    lt_student_update_x = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

    IF NOT lt_student_update IS INITIAL.

      SELECT * FROM zmstudent_um
      FOR ALL ENTRIES IN @lt_student_update
      WHERE id = @lt_student_update-id
      INTO TABLE @DATA(lt_student_update_old).

    ENDIF.

    gt_student = VALUE #(
    FOR x = 1 WHILE x <= lines( lt_student_update )

    LET
    ls_control_flag = VALUE #( lt_student_update_x[ x ] OPTIONAL )
    ls_student_new = VALUE #( lt_student_update[ x ] OPTIONAL )
    ls_student_old = VALUE #( lt_student_update_old[ id = ls_student_new-id ] OPTIONAL )
    IN
    (
    id = ls_student_new-id
    studentid = COND #( WHEN ls_control_flag-studentid IS NOT INITIAL
                        THEN ls_student_new-studentid ELSE ls_student_old-studentid )
    firstname = COND #( WHEN ls_control_flag-firstname IS NOT INITIAL
                        THEN ls_student_new-firstname ELSE ls_student_old-firstname )
    lastname = COND #( WHEN ls_control_flag-lastname IS NOT INITIAL
                        THEN ls_student_new-lastname ELSE ls_student_old-lastname )
    age = COND #( WHEN ls_control_flag-age IS NOT INITIAL
                        THEN ls_student_new-age ELSE ls_student_old-age )
    gender = COND #( WHEN ls_control_flag-gender IS NOT INITIAL
                        THEN ls_student_new-gender ELSE ls_student_old-gender )
    dob = COND #( WHEN ls_control_flag-dob IS NOT INITIAL
                        THEN ls_student_new-dob ELSE ls_student_old-dob )
    course = COND #( WHEN ls_control_flag-course IS NOT INITIAL
                        THEN ls_student_new-course ELSE ls_student_old-course )
    courseduration = COND #( WHEN ls_control_flag-courseduration IS NOT INITIAL
                        THEN ls_student_new-courseduration ELSE ls_student_old-courseduration )
    status = COND #( WHEN ls_control_flag-status IS NOT INITIAL
                        THEN ls_student_new-status ELSE ls_student_old-status )

    )
    ).



  ENDMETHOD.

  METHOD delete_student.

    DATA:lt_student TYPE STANDARD TABLE OF zmstudent_um.
    lt_student = CORRESPONDING #( keys MAPPING FROM ENTITY ).

    gr_student_d = VALUE #(

    FOR ls_student_d IN lt_student
    sign = 'I'
    option = 'EQ'
    (  low = ls_student_d-id )

    ).

  ENDMETHOD.

  METHOD earlynumbering_create_results.

    DATA(lv_new_results_id) = get_next_id( ).
    LOOP AT entities ASSIGNING FIELD-SYMBOL(<lfs_entities>).
      LOOP AT <lfs_entities>-%target ASSIGNING FIELD-SYMBOL(<lfs_result_create>).
        mapped-results = VALUE #(  (
                                    %cid = <lfs_result_create>-%cid
                                    %key = <lfs_result_create>-%key
                                    %is_draft = <lfs_result_create>-%is_draft
                ) ).

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD cba_results.

    gt_results = VALUE #(
    FOR ls_entities_cba IN entities_cba
    FOR ls_results_cba IN ls_entities_cba-%target
    LET
    ls_rap_results = CORRESPONDING zmacademic_um(
    ls_results_cba MAPPING FROM ENTITY
    )
    IN
    (
    ls_rap_results
    )
     ).

    mapped = VALUE #(
    results = VALUE #(
    FOR i = 1 WHILE i <= lines(  entities_cba )
    LET
    lt_results = VALUE #( entities_cba[ i ]-%target OPTIONAL )
    IN
    FOR j = 1 WHILE j <= lines( lt_results )
    LET
    ls_curr_results = VALUE #( lt_results[ j ] OPTIONAL )
    IN
    (
    %cid = ls_curr_results-%cid
    %key = ls_curr_results-%key
    Id = ls_curr_results-Id
    )
     )
      ).


  ENDMETHOD.

 METHOD rba_results.

*  DATA: lt_results TYPE STANDARD TABLE OF zmacademic_um.
*
*  SELECT *
*    FROM zmacademic_um  FOR ALL ENTRIES IN @keys_rba
*    WHERE id = @keys_rba-Id
*    INTO TABLE @lt_results.
*
*  result = CORRESPONDING #( lt_results MAPPING TO ENTITY ).







***  METHOD rba_Booking.
*    DATA: travel_out  TYPE /dmo/travel,
*          booking_out TYPE /dmo/t_booking,
*          booking     LIKE LINE OF result,
*          messages    TYPE /dmo/t_message.
*
*
*    LOOP AT keys_rba ASSIGNING FIELD-SYMBOL(<travel_rba>) GROUP BY <travel_rba>-Id.
*
*      CALL FUNCTION '/DMO/FLIGHT_TRAVEL_READ'
*        EXPORTING
*          iv_travel_id = <travel_rba>-Id
*        IMPORTING
*          es_travel    = travel_out
*          et_booking   = booking_out
*          et_messages  = messages.
*
*      map_messages(
*          EXPORTING
*            travel_id        = <travel_rba>-TravelID
*            messages         = messages
*            IMPORTING
*            failed_added = DATA(failed_added)
*          CHANGING
*            failed           = failed-travel
*            reported         = reported-travel
*        ).
*
*      IF failed_added = abap_false.
*        LOOP AT booking_out ASSIGNING FIELD-SYMBOL(<booking>).
*          "fill link table with key fields
*
*          INSERT
*            VALUE #(
*              source-%tky = <travel_rba>-%tky
*              target-%tky = VALUE #(
*                                TravelID  = <booking>-travel_id
*                                BookingID = <booking>-booking_id
*              ) )
*            INTO TABLE association_links.
*
*          IF result_requested = abap_true.
*            booking = CORRESPONDING #( <booking> MAPPING TO ENTITY ).
*            INSERT booking INTO TABLE result.
*          ENDIF.
*
*        ENDLOOP.
*      ENDIF.
*
*    ENDLOOP.
*
*    SORT association_links BY target ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM association_links COMPARING ALL FIELDS.
*
*    SORT result BY %tky ASCENDING.
*    DELETE ADJACENT DUPLICATES FROM result COMPARING ALL FIELDS.
  ENDMETHOD.






  METHOD read_results.

*"not triggering when it is in parent entity
*SELECT * FROM zmacademic_um FOR ALL ENTRIES IN @keys
*    WHERE id = @keys-Id
*    and course = @keys-Course
*    and semester = @keys-Semester
*    INTO TABLE @DATA(lt_result_data).
*
*    result = CORRESPONDING #( lt_result_data MAPPING TO ENTITY ).


  ENDMETHOD.

  METHOD delete_results.

   DATA:lt_results TYPE STANDARD TABLE OF zmacademic_um.
    lt_results = CORRESPONDING #( keys MAPPING FROM ENTITY ).

    gr_results_d = VALUE #(

    FOR ls_result_d IN lt_results
    sign = 'I'
    option = 'EQ'
    (  low = ls_result_d-id )

    ).

    gr_results_d_semester = VALUE #(
    FOR ls_result_d IN lt_results
    sign = 'I'
    option = 'EQ'
    ( low = ls_result_d-semester )
  ).

  gr_results_d_course = VALUE #(
    FOR ls_result_d IN lt_results
    sign = 'I'
    option = 'EQ'
    ( low = ls_result_d-course )
  ).



  ENDMETHOD.

  METHOD update_results.

DATA:lt_result_update   TYPE STANDARD TABLE OF zmacademic_um,
         lt_result_update_x TYPE STANDARD TABLE OF zmacademic_cs.

    lt_result_update = CORRESPONDING #( entities MAPPING FROM ENTITY ).
    lt_result_update_x = CORRESPONDING #( entities MAPPING FROM ENTITY USING CONTROL ).

    IF NOT lt_result_update IS INITIAL.

      SELECT * FROM zmacademic_um
      FOR ALL ENTRIES IN @lt_result_update
      WHERE id = @lt_result_update-id
      and course = @lt_result_update-course
      and semester = @lt_result_update-semester
      INTO TABLE @DATA(lt_result_update_old).

    ENDIF.

    gt_results = VALUE #(
    FOR x = 1 WHILE x <= lines( lt_result_update )

    LET
    ls_control_flag = VALUE #( lt_result_update_x[ x ] OPTIONAL )
    ls_result_new = VALUE #( lt_result_update[ x ] OPTIONAL )
    ls_result_old = VALUE #( lt_result_update_old[ id = ls_result_new-id ] OPTIONAL )
    IN
    (
    id = ls_result_new-id
    course = COND #( WHEN ls_control_flag-course IS NOT INITIAL
                        THEN ls_result_new-course ELSE ls_result_old-course )
    semester = COND #( WHEN ls_control_flag-semester IS NOT INITIAL
                        THEN ls_result_new-semester ELSE ls_result_old-semester )
    semresult = COND #( WHEN ls_control_flag-semresult IS NOT INITIAL
                        THEN ls_result_new-semresult ELSE ls_result_old-semresult )


    )
    ).

  ENDMETHOD.

ENDCLASS.
