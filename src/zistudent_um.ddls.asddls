@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'interface student'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Metadata.allowExtensions: true
define root view entity zistudent_um as select from zmstudent_um
association[0..*] to ZI_GENDER_RAP as _gender on $projection.Gender = _gender.Value
composition[0..*] of ziacademic_um as _results 
{
    key id as Id,
    studentid as StudentId,
    firstname as Firstname,
    lastname as Lastname,
    age as Age,
    course as Course,
    courseduration as Courseduration,
    status as Status,
    gender as Gender,
    dob as Dob,
    lastchangedat as Lastchangedat,
    locallastchangedat as Locallastchangedat,
    _gender.Description as genderDes,
    _gender,
    _results
}
