CLASS lhc_Student DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Student RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Student RESULT result.

    METHODS create FOR MODIFY
      IMPORTING entities FOR CREATE Student.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE Student.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Student.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Student.

    METHODS read FOR READ
      IMPORTING keys FOR READ Student RESULT result.

    METHODS lock FOR LOCK
      IMPORTING keys FOR LOCK Student.

    METHODS rba_Results FOR READ
      IMPORTING keys_rba FOR READ Student\_Results FULL result_requested RESULT result LINK association_links.

    METHODS cba_Results FOR MODIFY
      IMPORTING entities_cba FOR CREATE Student\_Results.

    METHODS setAdmitted FOR MODIFY
      IMPORTING keys FOR ACTION Student~setAdmitted RESULT result.

    METHODS validate_fields FOR VALIDATE ON SAVE
      IMPORTING keys FOR Student~validate_fields.
    METHODS updateCourse FOR DETERMINE ON SAVE
      IMPORTING keys FOR Student~updateCourse.
    METHODS updateCourseDes FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Student~updateCourseDes.
    METHODS UpdateStatus FOR MODIFY
      IMPORTING keys FOR ACTION Student~UpdateStatus RESULT result.

    METHODS earlynumbering_cba_Results FOR NUMBERING
      IMPORTING entities FOR CREATE Student\_Results.

ENDCLASS.

CLASS lhc_Student IMPLEMENTATION.

  METHOD get_instance_features.

  READ ENTITIES OF zistudent_um IN LOCAL MODE
    ENTITY Student
    FIELDS ( Status ) WITH CORRESPONDING #( keys )
    RESULT DATA(Studentadmitted)
    FAILED failed.
    result = VALUE #(
    FOR Stud IN Studentadmitted
    LET Statusval = COND #( WHEN Stud-Status = abap_true
                             THEN if_abap_behv=>fc-o-disabled
                             ELSE if_abap_behv=>fc-o-enabled )
                      IN ( %tky = stud-%tky
                      %action-setAdmitted = statusval )
    ).

  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD create.
   zcl_student_um=>get_instance( )->create_student(
     EXPORTING
       entities = entities
     CHANGING
       mapped   = mapped
       failed   = failed
       reported = reported
   ).
  ENDMETHOD.

  METHOD earlynumbering_create.

  zcl_student_um=>get_instance( )->earlynumbering_create(
    EXPORTING
      entities = entities
    CHANGING
      mapped   = mapped
      failed   = failed
      reported = reported
  ).

  ENDMETHOD.

  METHOD update.

  zcl_student_um=>get_instance( )->update_student(
    EXPORTING
      entities = entities
    CHANGING
      mapped   = mapped
      failed   = failed
      reported = reported
  ).

  ENDMETHOD.

  METHOD delete.

  zcl_student_um=>get_instance( )->delete_student(
    EXPORTING
      keys     = keys
    CHANGING
      mapped   = mapped
      failed   = failed
      reported = reported
  ).

  ENDMETHOD.

  METHOD read.

  zcl_student_um=>get_instance( )->read_student(
    EXPORTING
      keys     = keys
    CHANGING
      result   = result
      failed   = failed
      reported = reported
  ).

  ENDMETHOD.

  METHOD lock.

  TRY.
      DATA(lock) = cl_abap_lock_object_factory=>get_instance( iv_name = 'EZLOCKSTUDENT' ).
    CATCH cx_abap_lock_failure INTO DATA(exception).
      RAISE SHORTDUMP exception.
  ENDTRY.

  LOOP AT keys ASSIGNING FIELD-SYMBOL(<lfs_student>).

    TRY.
        lock->enqueue(
        it_parameter = VALUE #( ( name = 'ID' value = REF #( <lfs_student>-Id ) ) )
    ).

      CATCH cx_abap_foreign_lock INTO DATA(foreign_lock).

        APPEND VALUE #(
           id = keys[ 1 ]-Id
        %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Record is locked by' && foreign_lock->user_name
        )
        ) TO reported-student.

        APPEND VALUE #(
            id = keys[ 1 ]-Id
        ) TO failed-student.

      CATCH cx_abap_lock_failure INTO exception.
        RAISE SHORTDUMP exception.

    ENDTRY.
  ENDLOOP.


  ENDMETHOD.

  METHOD rba_Results.
*zcl_student_um=>get_instance( )->rba_results(
*  EXPORTING
*    keys_rba          = keys_rba
*    result_requested  = result_requested
*  CHANGING
*    result            = result
*    association_links = association_links
*    failed            = failed
*    reported          = reported
*).

  ENDMETHOD.

  METHOD cba_Results.

   zcl_student_um=>get_instance( )->cba_results(
     EXPORTING
       entities_cba = entities_cba
     CHANGING
       mapped       = mapped
       failed       = failed
       reported     = reported
   ).

  ENDMETHOD.

  METHOD earlynumbering_cba_Results.
  zcl_student_um=>get_instance( )->earlynumbering_create_results(
    EXPORTING
      entities = entities
    CHANGING
      mapped   = mapped
      failed   = failed
      reported = reported
  ).
  ENDMETHOD.

  METHOD setAdmitted.

   MODIFY ENTITIES OF zistudent_um IN LOCAL MODE
       ENTITY Student
       UPDATE
       FIELDS ( Status )
       WITH VALUE #( FOR key IN keys ( %tky = key-%tky Status = abap_true  ) )
       FAILED failed
       REPORTED reported.

    READ ENTITIES OF zistudent_um IN LOCAL MODE
    ENTITY Student
    ALL FIELDS WITH CORRESPONDING #( keys )
    RESULT DATA(studentdata).
    result = VALUE #( FOR studentrec IN studentdata
    ( %tky = studentrec-%tky %param = studentrec )
    ).

  ENDMETHOD.

  METHOD validate_fields.

  READ ENTITIES OF zistudent_um IN LOCAL MODE
    ENTITY Student
     all FIELDS  WITH CORRESPONDING #( keys )
     RESULT DATA(lt_student_tmp)
     reported data(lt_reported)
     failed data(lt_failed).

     if not lt_student_tmp[] is initial.

     read TABLE lt_student_tmp ASSIGNING FIELD-SYMBOL(<lfs_student_tmp>) index 1.
     if <lfs_student_tmp> is assigned .

     "to clear the previous validation errors.
     reported-student = value #(
     (  %tky = <lfs_student_tmp>-%tky %state_area = 'VALIDATE FNM' )
     (  %tky = <lfs_student_tmp>-%tky %state_area = 'VALIDATE AGE' )
     ).

     if <lfs_student_tmp>-Firstname is INITIAL  or <lfs_student_tmp>-Age is initial.

     failed-student = value #( ( %tky = <lfs_student_tmp>-%tky ) ).

     if <lfs_student_tmp>-Firstname is initial.
     reported-student = value #( (
     %tky = <lfs_student_tmp>-%tky
     %state_area = 'VALIDATE FNM' "which highlight the field with error
     %element-firstname = if_abap_behv=>mk-on "to check the field is whether changed or  not
     %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = 'Firstname  is required!!..'
            )
      ) ).
      endif.

*      if <lfs_student_tmp>-Lastname is initial.
*     reported-student = value #( BASE reported-student (
*     %tky = <lfs_student_tmp>-%tky
*     %state_area = 'VALIDATE LNM'
*     %element-lastname = if_abap_behv=>mk-on
*     %msg = new_message_with_text(
*              severity = if_abap_behv_message=>severity-error
*              text     = 'Lastname is required!!..'
*            )
*      ) ).
*
*      endif.

if <lfs_student_tmp>-Age is initial.
     reported-student = value #( BASE reported-student (
     %tky = <lfs_student_tmp>-%tky
     %state_area = 'VALIDATE AGE'
     %element-Age = if_abap_behv=>mk-on
     %msg = new_message_with_text(
              severity = if_abap_behv_message=>severity-error
              text     = 'Age is required!!..'
            )
      ) ).

      endif.




     endif.
     endif.

     endif.

  ENDMETHOD.

  METHOD updateCourse.

  READ ENTITIES OF zistudent_um IN LOCAL MODE
    ENTITY Student
     FIELDS ( Course )  WITH CORRESPONDING #( keys )
     RESULT DATA(lt_student_course)
     reported data(lt_reported)
     failed data(lt_failed).

     LOOP AT lt_student_course ASSIGNING FIELD-SYMBOL(<lfs_student_course>).

     if <lfs_student_course>-Course eq 'C'.
     MODIFY ENTITIES OF zistudent_um
     in LOCAL MODE
     ENTITY Student
     update FIELDS ( Courseduration )
     WITH VALUE #( ( %tky = <lfs_student_course>-%tky Courseduration = 5 ) ).
     endif.

     if <lfs_student_course>-Course eq 'E'.
     MODIFY ENTITIES OF zistudent_um
     in LOCAL MODE
     ENTITY Student
     update FIELDS ( Courseduration )
     WITH VALUE #( ( %tky = <lfs_student_course>-%tky Courseduration = 4 ) ).
     endif.

     if <lfs_student_course>-Course eq 'M'.
     MODIFY ENTITIES OF zistudent_um
     in LOCAL MODE
     ENTITY Student
     update FIELDS ( Courseduration )
     WITH VALUE #( ( %tky = <lfs_student_course>-%tky Courseduration = 6 ) ).
     endif.

     endloop.

  ENDMETHOD.

  METHOD updateCourseDes.

  READ ENTITIES OF zistudent_um IN LOCAL MODE
    ENTITY Student
     FIELDS ( Course )  WITH CORRESPONDING #( keys )
     RESULT DATA(lt_student_course)
     reported data(lt_reported)
     failed data(lt_failed).

     LOOP AT lt_student_course ASSIGNING FIELD-SYMBOL(<lfs_student_course>).

     if <lfs_student_course>-Course eq 'C'.
     MODIFY ENTITIES OF zistudent_um
     in LOCAL MODE
     ENTITY Student
     update FIELDS ( Courseduration )
     WITH VALUE #( ( %tky = <lfs_student_course>-%tky Courseduration = 5 ) ).
     endif.

     if <lfs_student_course>-Course eq 'E'.
     MODIFY ENTITIES OF zistudent_um
     in LOCAL MODE
     ENTITY Student
     update FIELDS ( Courseduration )
     WITH VALUE #( ( %tky = <lfs_student_course>-%tky Courseduration = 4 ) ).
     endif.

     if <lfs_student_course>-Course eq 'M'.
     MODIFY ENTITIES OF zistudent_um
     in LOCAL MODE
     ENTITY Student
     update FIELDS ( Courseduration )
     WITH VALUE #( ( %tky = <lfs_student_course>-%tky Courseduration = 6 ) ).
     endif.

     endloop.


 ENDMETHOD.

  METHOD UpdateStatus.

  data(lt_keys) = keys.

  READ ENTITIES OF zistudent_um
  in LOCAL MODE
  ENTITY Student
  FIELDS ( Status Gender ) with CORRESPONDING #( keys )
  RESULT DATA(lt_student_cs).

    data(lv_new_status) = lt_keys[ 1 ]-%param-Status.
     data(lv_new_gender) = lt_keys[ 1 ]-%param-gender.

    MODIFY ENTITIES OF zistudent_um
    in LOCAL MODE
    entity Student
    update fields ( Status Gender )
    with value #( (
    %tky = lt_student_cs[ 1 ]-%tky Status = lv_new_status
    Gender = lv_new_gender
     ) ).

    READ ENTITIES OF zistudent_um
  in LOCAL MODE
  ENTITY Student
  all FIELDS  with CORRESPONDING #( keys )
  RESULT DATA(lt_student).

  result = value #(
  for <lfs_student> in lt_student (
  %tky = <lfs_student>-%tky
  %param = <lfs_student>
   )
   ).

  ENDMETHOD.

ENDCLASS.

CLASS lhc_Results DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS update FOR MODIFY
      IMPORTING entities FOR UPDATE Results.

    METHODS delete FOR MODIFY
      IMPORTING keys FOR DELETE Results.

    METHODS read FOR READ
      IMPORTING keys FOR READ Results RESULT result.

    METHODS rba_Student FOR READ
      IMPORTING keys_rba FOR READ Results\_Student FULL result_requested RESULT result LINK association_links.

ENDCLASS.

CLASS lhc_Results IMPLEMENTATION.

  METHOD update.

  zcl_student_um=>get_instance( )->update_results(
    EXPORTING
      entities = entities
    CHANGING
      mapped   = mapped
      failed   = failed
      reported = reported
  ).

  ENDMETHOD.

  METHOD delete.

  zcl_student_um=>get_instance( )->delete_results(
    EXPORTING
      keys     = keys
    CHANGING
      mapped   = mapped
      failed   = failed
      reported = reported
  ).

  ENDMETHOD.

  METHOD read.
  ENDMETHOD.

  METHOD rba_Student.
  ENDMETHOD.

ENDCLASS.

CLASS lsc_ZISTUDENT_UM DEFINITION INHERITING FROM cl_abap_behavior_saver.
  PROTECTED SECTION.

    METHODS finalize REDEFINITION.

    METHODS check_before_save REDEFINITION.

    METHODS save REDEFINITION.

    METHODS cleanup REDEFINITION.

    METHODS cleanup_finalize REDEFINITION.

ENDCLASS.

CLASS lsc_ZISTUDENT_UM IMPLEMENTATION.

  METHOD finalize.
  ENDMETHOD.

  METHOD check_before_save.



  DATA: gt_student_tmp TYPE STANDARD TABLE OF zmstudent_um.
  gt_student_tmp = zcl_student_um=>get_instance( )->gt_student.

  IF NOT gt_student_tmp IS INITIAL.

    READ TABLE gt_student_tmp ASSIGNING FIELD-SYMBOL(<lfs_student_tmp>) INDEX 1.
    IF <lfs_student_tmp> IS ASSIGNED.
      IF <lfs_student_tmp>-age < 22.
        APPEND VALUE #( id = <lfs_student_tmp>-id ) TO failed-student.

        APPEND VALUE #(
          id = <lfs_student_tmp>-id
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Age is less than minimum required 22'
          )
        ) TO reported-student.
      ENDIF.
    ENDIF.

*    IF <lfs_student_tmp>-status EQ abap_false.
*      APPEND VALUE #( id = <lfs_student_tmp>-id ) TO failed-student.
*
*      APPEND VALUE #(
*        id = <lfs_student_tmp>-id
*        %msg = new_message_with_text(
*          severity = if_abap_behv_message=>severity-error
*          text = 'Set Status to Active'
*        )
*      ) TO reported-student.
*    ENDIF.

IF <lfs_student_tmp>-firstname IS INITIAL.
        APPEND VALUE #( id = <lfs_student_tmp>-id ) TO failed-student.
        APPEND VALUE #(
          id = <lfs_student_tmp>-id
          %msg = new_message_with_text(
            severity = if_abap_behv_message=>severity-error
            text = 'Student Name is mandatory'
          )
        ) TO reported-student.
      ENDIF.


  ENDIF.

  ENDMETHOD.

  METHOD save.

   zcl_student_um=>get_instance( )->savedata(
     CHANGING
       reported = reported
   ).

  ENDMETHOD.

  METHOD cleanup.
  ENDMETHOD.

  METHOD cleanup_finalize.
  ENDMETHOD.

ENDCLASS.
