@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'consumption academic'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity zcacademic_um as projection on ziacademic_um
{
    key Id,
    key Course,
    key Semester,
    Course_desc,
    Semester_desc,
    Semresult,
    Semres_desc,
    /* Associations */
    _student : redirected to parent zcstudent_um
}
