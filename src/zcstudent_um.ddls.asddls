@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'student consumption'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity zcstudent_um
  provider contract transactional_query as projection on zistudent_um
{
    key Id,
    StudentId,
    Firstname,
    Lastname,
    Age,
    Course,
    Courseduration,
    Status,
    Gender,
    Dob,
    Lastchangedat,
    Locallastchangedat,
    genderDes,
    /* Associations */
    _gender ,
    _results : redirected to composition child zcacademic_um
}
