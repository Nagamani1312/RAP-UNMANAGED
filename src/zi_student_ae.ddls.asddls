@EndUserText.label: 'abstract entity'
@Metadata.allowExtensions: true
define abstract entity ZI_STUDENT_AE
 // with parameters parameter_name : parameter_type
{
//@UI.defaultValue: 'X'
@UI.defaultValue: #( ' ELEMENT_OF_REFERENCED_ENTITY: Status' )
    Status : abap_boolean;
@UI.defaultValue: #( ' ELEMENT_OF_REFERENCED_ENTITY: Gender' )    
    gender             : abap.char(1);
}
