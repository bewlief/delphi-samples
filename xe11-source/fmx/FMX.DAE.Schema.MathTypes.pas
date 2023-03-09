{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.DAE.Schema.MathTypes;

interface
uses
  XML.XMLDoc, XML.XMLIntf, FMX.DAE.Schema;

type

 TXMLMathTextBaseType = class(TXMLNode)
 protected
    function Get_Mi: IXMLMitype;
    function Get_Mo: IXMLMotype;
    function Get_Mn: IXMLMntype;
    function Get_Mtext: IXMLMtexttype;
    function Get_Ms: IXMLMstype;
    function Get_Mrow: IXMLMrowtype;
    function Get_Mfrac: IXMLMfractype;
    function Get_Msqrt: IXMLMsqrttype;
    function Get_Mroot: IXMLMroottype;
    function Get_Mpadded: IXMLMpaddedtype;
    function Get_Mphantom: IXMLMphantomtype;
    function Get_Mfenced: IXMLMfencedtype;
    function Get_Menclose: IXMLMenclosetype;
    function Get_Msub: IXMLMsubtype;
    function Get_Msup: IXMLMsuptype;
    function Get_Msubsup: IXMLMsubsuptype;
    function Get_Munder: IXMLMundertype;
    function Get_Mover: IXMLMovertype;
    function Get_Munderover: IXMLMunderovertype;
    function Get_Mmultiscripts: IXMLMmultiscriptstype;
    function Get_Mtable: IXMLMtabletype;
    function Get_Maligngroup: IXMLMaligngrouptype;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mspace: IXMLMspacetype;
    function Get_Maction: IXMLMactiontype;
    function Get_Merror: IXMLMerrortype;
    function Get_Mstyle: IXMLMstyletype;

 public
    procedure AfterConstruction; override;
 end;


 TXMLMathBaseType = class(TXMLMathTextBaseType)
 protected
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;


    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;

    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;



    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;

    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;
 public
    procedure AfterConstruction; override;
 end;


{ TXMLMathtype }

  TXMLMathtype = class(TXMLMathBaseType, IXMLMathtype)
  protected
    { IXMLMathtype }
    function Get_Baseline: UnicodeString;
    function Get_Overflow: UnicodeString;
    function Get_Altimg: UnicodeString;
    function Get_Alttext: UnicodeString;
    function Get_Type_: UnicodeString;
    function Get_Name: UnicodeString;
    function Get_Height: UnicodeString;
    function Get_Width: UnicodeString;
    function Get_Macros: UnicodeString;
    function Get_Display: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    //
    procedure Set_Baseline(Value: UnicodeString);
    procedure Set_Overflow(Value: UnicodeString);
    procedure Set_Altimg(Value: UnicodeString);
    procedure Set_Alttext(Value: UnicodeString);
    procedure Set_Type_(Value: UnicodeString);
    procedure Set_Name(Value: UnicodeString);
    procedure Set_Height(Value: UnicodeString);
    procedure Set_Width(Value: UnicodeString);
    procedure Set_Macros(Value: UnicodeString);
    procedure Set_Display(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;
{ TXMLCntype }

  TXMLCntype = class(TXMLMathBaseType, IXMLCntype)
  protected
    { IXMLCntype }
    function Get_Base: LongWord;
    function Get_Type_: UnicodeString;
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
//
    function Get_Sep: IXMLSeptype;
    procedure Set_Base(Value: LongWord);
    procedure Set_Type_(Value: UnicodeString);
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

  { TXMLCitype }

  TXMLCitype = class(TXMLMathBaseType, IXMLCitype)
  protected
    { IXMLCitype }
    function Get_Type_: UnicodeString;
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
//
    procedure Set_Type_(Value: UnicodeString);
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCsymboltype }

  TXMLCsymboltype = class(TXMLMathBaseType, IXMLCsymboltype)
  protected
    { IXMLCsymboltype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
//
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLArithtype }

  TXMLArithtype = class(TXMLNode, IXMLArithtype)
  protected
    { IXMLArithtype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLFunctionstype }

  TXMLFunctionstype = class(TXMLNode, IXMLFunctionstype)
  protected
    { IXMLFunctionstype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;


implementation
type

{ TXMLMtrtype }

  TXMLMtrtype = class(TXMLNode, IXMLMtrtype)
  protected
    { IXMLMtrtype }
    function Get_Rowalign: UnicodeString;
    function Get_Columnalign: UnicodeString;
    function Get_Groupalign: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Mtd: IXMLMtdtype;
    procedure Set_Rowalign(Value: UnicodeString);
    procedure Set_Columnalign(Value: UnicodeString);
    procedure Set_Groupalign(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

  { TXMLMlabeledtrtype }

  TXMLMlabeledtrtype = class(TXMLNode, IXMLMlabeledtrtype)
  protected
    { IXMLMlabeledtrtype }
    function Get_Rowalign: UnicodeString;
    function Get_Columnalign: UnicodeString;
    function Get_Groupalign: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Mtd: IXMLMtdtype;
    procedure Set_Rowalign(Value: UnicodeString);
    procedure Set_Columnalign(Value: UnicodeString);
    procedure Set_Groupalign(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLAnnotationtype }

  TXMLAnnotationtype = class(TXMLNode, IXMLAnnotationtype)
  protected
    { IXMLAnnotationtype }
    function Get_Encoding: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLAnnotationxmltype }

  TXMLAnnotationxmltype = class(TXMLNode, IXMLAnnotationxmltype)
  protected
    { IXMLAnnotationxmltype }
    function Get_Encoding: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMglyphtype }

  TXMLMglyphtype = class(TXMLNode, IXMLMglyphtype)
  protected
    { IXMLMglyphtype }
    function Get_Alt: UnicodeString;
    function Get_Fontfamily: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Alt(Value: UnicodeString);
    procedure Set_Fontfamily(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;


 { TXMLNonetype }

  TXMLNonetype = class(TXMLNode, IXMLNonetype)
  protected
    { IXMLNonetype }
  end;

{ TXMLMprescriptstype }

  TXMLMprescriptstype = class(TXMLNode, IXMLMprescriptstype)
  protected
    { IXMLMprescriptstype }
  end;

{ TXMLSeptype }

  TXMLSeptype = class(TXMLNode, IXMLSeptype)
  protected
    { IXMLSeptype }
  end;


{ TXMLPiecetype }

  TXMLPiecetype = class(TXMLMathBaseType, IXMLPiecetype)
  protected
    { IXMLPiecetype }
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPiecetypeList }

  TXMLPiecetypeList = class(TXMLNodeCollection, IXMLPiecetypeList)
  protected
    { IXMLPiecetypeList }
    function Add: IXMLPiecetype;
    function Insert(const Index: Integer): IXMLPiecetype;

    function Get_Item(Index: Integer): IXMLPiecetype;
  end;


{ TXMLOtherwisetype }

  TXMLOtherwisetype = class(TXMLMathBaseType, IXMLOtherwisetype)
  protected
    { IXMLOtherwisetype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSemanticstype }

  TXMLSemanticstype = class(TXMLMathTextBaseType, IXMLSemanticstype)
  protected
    { IXMLSemanticstype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;
    function Get_Annotation: IXMLAnnotationtype;
    function Get_Annotationxml: IXMLAnnotationxmltype;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBvartype }

  TXMLBvartype = class(TXMLMathTextBaseType, IXMLBvartype)
  protected
    { IXMLBvartype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDegreetype }

  TXMLDegreetype = class(TXMLMathTextBaseType, IXMLDegreetype)
  protected
    { IXMLDegreetype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLApplytype }

  TXMLApplytype = class(TXMLMathTextBaseType, IXMLApplytype)
  protected
    { IXMLApplytype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLIntervaltype }

  TXMLIntervaltype = class(TXMLMathBaseType, IXMLIntervaltype)
  protected
    { IXMLIntervaltype }
    function Get_Closure: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;


    procedure Set_Closure(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLModetype }

  TXMLModetype = class(TXMLNode, IXMLModetype)
  protected
    { IXMLModetype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMomenttype }

  TXMLMomenttype = class(TXMLNode, IXMLMomenttype)
  protected
    { IXMLMomenttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLInversetype }

  TXMLInversetype = class(TXMLNode, IXMLInversetype)
  protected
    { IXMLInversetype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;


{ TXMLMeantype }

  TXMLMeantype = class(TXMLNode, IXMLMeantype)
  protected
    { IXMLMeantype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLSdevtype }

  TXMLSdevtype = class(TXMLNode, IXMLSdevtype)
  protected
    { IXMLSdevtype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLVariancetype }

  TXMLVariancetype = class(TXMLNode, IXMLVariancetype)
  protected
    { IXMLVariancetype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMediantype }

  TXMLMediantype = class(TXMLNode, IXMLMediantype)
  protected
    { IXMLMediantype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;


{ TXMLScalarproducttype }

  TXMLScalarproducttype = class(TXMLNode, IXMLScalarproducttype)
  protected
    { IXMLScalarproducttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLOuterproducttype }

  TXMLOuterproducttype = class(TXMLNode, IXMLOuterproducttype)
  protected
    { IXMLOuterproducttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLDivergencetype }

  TXMLDivergencetype = class(TXMLNode, IXMLDivergencetype)
  protected
    { IXMLDivergencetype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLGradtype }

  TXMLGradtype = class(TXMLNode, IXMLGradtype)
  protected
    { IXMLGradtype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLCurltype }

  TXMLCurltype = class(TXMLNode, IXMLCurltype)
  protected
    { IXMLCurltype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLLaplaciantype }

  TXMLLaplaciantype = class(TXMLNode, IXMLLaplaciantype)
  protected
    { IXMLLaplaciantype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLVectorproducttype }

  TXMLVectorproducttype = class(TXMLNode, IXMLVectorproducttype)
  protected
    { IXMLVectorproducttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLTransposetype }

  TXMLTransposetype = class(TXMLNode, IXMLTransposetype)
  protected
    { IXMLTransposetype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLSelectortype }

  TXMLSelectortype = class(TXMLNode, IXMLSelectortype)
  protected
    { IXMLSelectortype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMomentabouttype }

  TXMLMomentabouttype = class(TXMLMathTextBaseType, IXMLMomentabouttype)
  protected
    { IXMLMomentabouttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMenclosetype }

  TXMLMenclosetype = class(TXMLMathBaseType, IXMLMenclosetype)
  protected
    { IXMLMenclosetype }
    function Get_Notation: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Notation(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;




{ TXMLMfencedtype }

  TXMLMfencedtype = class(TXMLMathBaseType, IXMLMfencedtype)
  protected
    { IXMLMfencedtype }
    function Get_Open: UnicodeString;
    function Get_Close: UnicodeString;
    function Get_Separators: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Open(Value: UnicodeString);
    procedure Set_Close(Value: UnicodeString);
    procedure Set_Separators(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{TXMLMrowtype }

  TXMLMrowtype = class(TXMLMathBaseType, IXMLMrowtype)
  protected
    { IXMLMrowtype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMpaddedtype }

  TXMLMpaddedtype = class(TXMLMathBaseType, IXMLMpaddedtype)
  protected
    { IXMLMpaddedtype }
    function Get_Width: UnicodeString;
    function Get_Lspace: UnicodeString;
    function Get_Height: UnicodeString;
    function Get_Depth: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Width(Value: UnicodeString);
    procedure Set_Lspace(Value: UnicodeString);
    procedure Set_Height(Value: UnicodeString);
    procedure Set_Depth(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMphantomtype }

  TXMLMphantomtype = class(TXMLMathBaseType, IXMLMphantomtype)
  protected
    { IXMLMphantomtype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMroottype }

  TXMLMroottype = class(TXMLMathBaseType, IXMLMroottype)
  protected
    { IXMLMroottype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMitype }

  TXMLMitype = class(TXMLNode, IXMLMitype)
  protected
    { IXMLMitype }
    function Get_Mathvariant: UnicodeString;
    function Get_Mathsize: UnicodeString;
    function Get_Mathcolor: UnicodeString;
    function Get_Mathbackground: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mglyph: IXMLMglyphtype;
    procedure Set_Mathvariant(Value: UnicodeString);
    procedure Set_Mathsize(Value: UnicodeString);
    procedure Set_Mathcolor(Value: UnicodeString);
    procedure Set_Mathbackground(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMalignmarktype }

  TXMLMalignmarktype = class(TXMLNode, IXMLMalignmarktype)
  protected
    { IXMLMalignmarktype }
    function Get_Edge: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Edge(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMotype }

  TXMLMotype = class(TXMLNode, IXMLMotype)
  protected
    { IXMLMotype }
    function Get_Form: UnicodeString;
    function Get_Lspace: UnicodeString;
    function Get_Rspace: UnicodeString;
    function Get_Fence: Boolean;
    function Get_Separator: Boolean;
    function Get_Stretchy: Boolean;
    function Get_Symmetric: Boolean;
    function Get_Movablelimits: Boolean;
    function Get_Accent: Boolean;
    function Get_Largeop: Boolean;
    function Get_Minsize: UnicodeString;
    function Get_Maxsize: UnicodeString;
    function Get_Mathvariant: UnicodeString;
    function Get_Mathsize: UnicodeString;
    function Get_Mathcolor: UnicodeString;
    function Get_Mathbackground: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mglyph: IXMLMglyphtype;
    procedure Set_Form(Value: UnicodeString);
    procedure Set_Lspace(Value: UnicodeString);
    procedure Set_Rspace(Value: UnicodeString);
    procedure Set_Fence(Value: Boolean);
    procedure Set_Separator(Value: Boolean);
    procedure Set_Stretchy(Value: Boolean);
    procedure Set_Symmetric(Value: Boolean);
    procedure Set_Movablelimits(Value: Boolean);
    procedure Set_Accent(Value: Boolean);
    procedure Set_Largeop(Value: Boolean);
    procedure Set_Minsize(Value: UnicodeString);
    procedure Set_Maxsize(Value: UnicodeString);
    procedure Set_Mathvariant(Value: UnicodeString);
    procedure Set_Mathsize(Value: UnicodeString);
    procedure Set_Mathcolor(Value: UnicodeString);
    procedure Set_Mathbackground(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMntype }

  TXMLMntype = class(TXMLNode, IXMLMntype)
  protected
    { IXMLMntype }
    function Get_Mathvariant: UnicodeString;
    function Get_Mathsize: UnicodeString;
    function Get_Mathcolor: UnicodeString;
    function Get_Mathbackground: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mglyph: IXMLMglyphtype;
    procedure Set_Mathvariant(Value: UnicodeString);
    procedure Set_Mathsize(Value: UnicodeString);
    procedure Set_Mathcolor(Value: UnicodeString);
    procedure Set_Mathbackground(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLMtexttype }

  TXMLMtexttype = class(TXMLNode, IXMLMtexttype)
  protected
    { IXMLMtexttype }
    function Get_Mathvariant: UnicodeString;
    function Get_Mathsize: UnicodeString;
    function Get_Mathcolor: UnicodeString;
    function Get_Mathbackground: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mglyph: IXMLMglyphtype;
    procedure Set_Mathvariant(Value: UnicodeString);
    procedure Set_Mathsize(Value: UnicodeString);
    procedure Set_Mathcolor(Value: UnicodeString);
    procedure Set_Mathbackground(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


  { TXMLMstype }

  TXMLMstype = class(TXMLNode, IXMLMstype)
  protected
    { IXMLMstype }
    function Get_Lquote: UnicodeString;
    function Get_Rquote: UnicodeString;
    function Get_Mathvariant: UnicodeString;
    function Get_Mathsize: UnicodeString;
    function Get_Mathcolor: UnicodeString;
    function Get_Mathbackground: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mglyph: IXMLMglyphtype;
    procedure Set_Lquote(Value: UnicodeString);
    procedure Set_Rquote(Value: UnicodeString);
    procedure Set_Mathvariant(Value: UnicodeString);
    procedure Set_Mathsize(Value: UnicodeString);
    procedure Set_Mathcolor(Value: UnicodeString);
    procedure Set_Mathbackground(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLMtabletype }

  TXMLMtabletype = class(TXMLNode, IXMLMtabletype)
  protected
    { IXMLMtabletype }
    function Get_Rowalign: UnicodeString;
    function Get_Columnalign: UnicodeString;
    function Get_Groupalign: UnicodeString;
    function Get_Align: UnicodeString;
    function Get_Alignmentscope: UnicodeString;
    function Get_Columnwidth: UnicodeString;
    function Get_Width: UnicodeString;
    function Get_Rowspacing: UnicodeString;
    function Get_Columnspacing: UnicodeString;
    function Get_Rowlines: UnicodeString;
    function Get_Columnlines: UnicodeString;
    function Get_Frame: UnicodeString;
    function Get_Framespacing: UnicodeString;
    function Get_Equalrows: Boolean;
    function Get_Equalcolumns: Boolean;
    function Get_Displaystyle: Boolean;
    function Get_Side: UnicodeString;
    function Get_Minlabelspacing: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Mtr: IXMLMtrtype;
    function Get_Mlabeledtr: IXMLMlabeledtrtype;
    procedure Set_Rowalign(Value: UnicodeString);
    procedure Set_Columnalign(Value: UnicodeString);
    procedure Set_Groupalign(Value: UnicodeString);
    procedure Set_Align(Value: UnicodeString);
    procedure Set_Alignmentscope(Value: UnicodeString);
    procedure Set_Columnwidth(Value: UnicodeString);
    procedure Set_Width(Value: UnicodeString);
    procedure Set_Rowspacing(Value: UnicodeString);
    procedure Set_Columnspacing(Value: UnicodeString);
    procedure Set_Rowlines(Value: UnicodeString);
    procedure Set_Columnlines(Value: UnicodeString);
    procedure Set_Frame(Value: UnicodeString);
    procedure Set_Framespacing(Value: UnicodeString);
    procedure Set_Equalrows(Value: Boolean);
    procedure Set_Equalcolumns(Value: Boolean);
    procedure Set_Displaystyle(Value: Boolean);
    procedure Set_Side(Value: UnicodeString);
    procedure Set_Minlabelspacing(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLConditiontype }

  TXMLConditiontype = class(TXMLMathTextBaseType, IXMLConditiontype)
  protected
    { IXMLConditiontype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDeclaretype }

  TXMLDeclaretype = class(TXMLMathTextBaseType, IXMLDeclaretype)
  protected
    { IXMLDeclaretype }
    function Get_Type_: UnicodeString;
    function Get_Scope: UnicodeString;
    function Get_Nargs: LongWord;
    function Get_Occurrence: UnicodeString;
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Type_(Value: UnicodeString);
    procedure Set_Scope(Value: UnicodeString);
    procedure Set_Nargs(Value: LongWord);
    procedure Set_Occurrence(Value: UnicodeString);
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLLambdatype }

  TXMLLambdatype = class(TXMLMathTextBaseType, IXMLLambdatype)
  protected
    { IXMLLambdatype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPiecewisetype }

  TXMLPiecewisetype = class(TXMLNode, IXMLPiecewisetype)
  private
    FPiece: IXMLPiecetypeList;
  protected
    { IXMLPiecewisetype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Piece: IXMLPiecetypeList;
    function Get_Otherwise: IXMLOtherwisetype;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLMfractype }

  TXMLMfractype = class(TXMLMathBaseType, IXMLMfractype)
  protected
    { IXMLMfractype }
    function Get_Bevelled: Boolean;
    function Get_Denomalign: UnicodeString;
    function Get_Numalign: UnicodeString;
    function Get_Linethickness: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Bevelled(Value: Boolean);
    procedure Set_Denomalign(Value: UnicodeString);
    procedure Set_Numalign(Value: UnicodeString);
    procedure Set_Linethickness(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMsqrttype }

  TXMLMsqrttype = class(TXMLMathBaseType, IXMLMsqrttype)
  protected
    { IXMLMsqrttype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMsubtype }

  TXMLMsubtype = class(TXMLMathBaseType, IXMLMsubtype)
  protected
    { IXMLMsubtype }
    function Get_Subscriptshift: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Subscriptshift(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMsuptype }

  TXMLMsuptype = class(TXMLMathBaseType, IXMLMsuptype)
  protected
    { IXMLMsuptype }
    function Get_Superscriptshift: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Superscriptshift(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMsubsuptype }

  TXMLMsubsuptype = class(TXMLMathBaseType, IXMLMsubsuptype)
  protected
    { IXMLMsubsuptype }
    function Get_Subscriptshift: UnicodeString;
    function Get_Superscriptshift: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Subscriptshift(Value: UnicodeString);
    procedure Set_Superscriptshift(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMundertype }

  TXMLMundertype = class(TXMLMathBaseType, IXMLMundertype)
  protected
    { IXMLMundertype }
    function Get_Accentunder: Boolean;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Accentunder(Value: Boolean);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMovertype }

  TXMLMovertype = class(TXMLMathBaseType, IXMLMovertype)
  protected
    { IXMLMovertype }
    function Get_Accent: Boolean;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Accent(Value: Boolean);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMunderovertype }

  TXMLMunderovertype = class(TXMLMathBaseType, IXMLMunderovertype)
  protected
    { IXMLMunderovertype }
    function Get_Accent: Boolean;
    function Get_Accentunder: Boolean;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Accent(Value: Boolean);
    procedure Set_Accentunder(Value: Boolean);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMmultiscriptstype }

  TXMLMmultiscriptstype = class(TXMLMathBaseType, IXMLMmultiscriptstype)
  protected
    { IXMLMmultiscriptstype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_None: IXMLNonetype;

    function Get_Mprescripts: IXMLMprescriptstype;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMtdtype }

  TXMLMtdtype = class(TXMLMathBaseType, IXMLMtdtype)
  protected
    { IXMLMtdtype }
    function Get_Rowalign: UnicodeString;
    function Get_Columnalign: UnicodeString;
    function Get_Groupalign: UnicodeString;
    function Get_Columnspan: LongWord;
    function Get_Rowspan: LongWord;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Rowalign(Value: UnicodeString);
    procedure Set_Columnalign(Value: UnicodeString);
    procedure Set_Groupalign(Value: UnicodeString);
    procedure Set_Columnspan(Value: LongWord);
    procedure Set_Rowspan(Value: LongWord);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMaligngrouptype }

  TXMLMaligngrouptype = class(TXMLNode, IXMLMaligngrouptype)
  protected
    { IXMLMaligngrouptype }
    function Get_Groupalign: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Groupalign(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMspacetype }

  TXMLMspacetype = class(TXMLNode, IXMLMspacetype)
  protected
    { IXMLMspacetype }
    function Get_Width: UnicodeString;
    function Get_Height: UnicodeString;
    function Get_Depth: UnicodeString;
    function Get_Linebreak: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Width(Value: UnicodeString);
    procedure Set_Height(Value: UnicodeString);
    procedure Set_Depth(Value: UnicodeString);
    procedure Set_Linebreak(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLMactiontype }

  TXMLMactiontype = class(TXMLMathBaseType, IXMLMactiontype)
  protected
    { IXMLMactiontype }
    function Get_Actiontype: UnicodeString;
    function Get_Selection: LongWord;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Actiontype(Value: UnicodeString);
    procedure Set_Selection(Value: LongWord);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMerrortype }

  TXMLMerrortype = class(TXMLMathBaseType, IXMLMerrortype)
  protected
    { IXMLMerrortype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMstyletype }

  TXMLMstyletype = class(TXMLMathBaseType, IXMLMstyletype)
  protected
    { IXMLMstyletype }
    function Get_Scriptlevel: Integer;
    function Get_Displaystyle: Boolean;
    function Get_Scriptsizemultiplier: UnicodeString;
    function Get_Scriptminsize: UnicodeString;
    function Get_Color: UnicodeString;
    function Get_Background: UnicodeString;
    function Get_Veryverythinmathspace: UnicodeString;
    function Get_Verythinmathspace: UnicodeString;
    function Get_Thinmathspace: UnicodeString;
    function Get_Mediummathspace: UnicodeString;
    function Get_Thickmathspace: UnicodeString;
    function Get_Verythickmathspace: UnicodeString;
    function Get_Veryverythickmathspace: UnicodeString;
    function Get_Linethickness: UnicodeString;
    function Get_Form: UnicodeString;
    function Get_Lspace: UnicodeString;
    function Get_Rspace: UnicodeString;
    function Get_Fence: Boolean;
    function Get_Separator: Boolean;
    function Get_Stretchy: Boolean;
    function Get_Symmetric: Boolean;
    function Get_Movablelimits: Boolean;
    function Get_Accent: Boolean;
    function Get_Largeop: Boolean;
    function Get_Minsize: UnicodeString;
    function Get_Maxsize: UnicodeString;
    function Get_Mathvariant: UnicodeString;
    function Get_Mathsize: UnicodeString;
    function Get_Mathcolor: UnicodeString;
    function Get_Mathbackground: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;

    procedure Set_Scriptlevel(Value: Integer);
    procedure Set_Displaystyle(Value: Boolean);
    procedure Set_Scriptsizemultiplier(Value: UnicodeString);
    procedure Set_Scriptminsize(Value: UnicodeString);
    procedure Set_Color(Value: UnicodeString);
    procedure Set_Background(Value: UnicodeString);
    procedure Set_Veryverythinmathspace(Value: UnicodeString);
    procedure Set_Verythinmathspace(Value: UnicodeString);
    procedure Set_Thinmathspace(Value: UnicodeString);
    procedure Set_Mediummathspace(Value: UnicodeString);
    procedure Set_Thickmathspace(Value: UnicodeString);
    procedure Set_Verythickmathspace(Value: UnicodeString);
    procedure Set_Veryverythickmathspace(Value: UnicodeString);
    procedure Set_Linethickness(Value: UnicodeString);
    procedure Set_Form(Value: UnicodeString);
    procedure Set_Lspace(Value: UnicodeString);
    procedure Set_Rspace(Value: UnicodeString);
    procedure Set_Fence(Value: Boolean);
    procedure Set_Separator(Value: Boolean);
    procedure Set_Stretchy(Value: Boolean);
    procedure Set_Symmetric(Value: Boolean);
    procedure Set_Movablelimits(Value: Boolean);
    procedure Set_Accent(Value: Boolean);
    procedure Set_Largeop(Value: Boolean);
    procedure Set_Minsize(Value: UnicodeString);
    procedure Set_Maxsize(Value: UnicodeString);
    procedure Set_Mathvariant(Value: UnicodeString);
    procedure Set_Mathsize(Value: UnicodeString);
    procedure Set_Mathcolor(Value: UnicodeString);
    procedure Set_Mathbackground(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;



{ TXMLDomainofapplicationtype }

  TXMLDomainofapplicationtype = class(TXMLMathTextBaseType, IXMLDomainofapplicationtype)
  protected
    { IXMLDomainofapplicationtype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLElementaryfunctionstype }

  TXMLElementaryfunctionstype = class(TXMLNode, IXMLElementaryfunctionstype)
  protected
    { IXMLElementaryfunctionstype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLLogictype }

  TXMLLogictype = class(TXMLNode, IXMLLogictype)
  protected
    { IXMLLogictype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLConstanttype }

  TXMLConstanttype = class(TXMLNode, IXMLConstanttype)
  protected
    { IXMLConstanttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLSettype }

  TXMLSettype = class(TXMLMathTextBaseType, IXMLSettype)
  protected
    { IXMLSettype }
    function Get_Type_: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Type_(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLListtype }

  TXMLListtype = class(TXMLMathTextBaseType, IXMLListtype)
  protected
    { IXMLListtype }
    function Get_Order: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Order(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLUniontype }

  TXMLUniontype = class(TXMLNode, IXMLUniontype)
  protected
    { IXMLUniontype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLIntersecttype }

  TXMLIntersecttype = class(TXMLNode, IXMLIntersecttype)
  protected
    { IXMLIntersecttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLIntype }

  TXMLIntype = class(TXMLNode, IXMLIntype)
  protected
    { IXMLIntype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLNotintype }

  TXMLNotintype = class(TXMLNode, IXMLNotintype)
  protected
    { IXMLNotintype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLSubsettype }

  TXMLSubsettype = class(TXMLNode, IXMLSubsettype)
  protected
    { IXMLSubsettype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLPrsubsettype }

  TXMLPrsubsettype = class(TXMLNode, IXMLPrsubsettype)
  protected
    { IXMLPrsubsettype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLNotsubsettype }

  TXMLNotsubsettype = class(TXMLNode, IXMLNotsubsettype)
  protected
    { IXMLNotsubsettype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLNotprsubsettype }

  TXMLNotprsubsettype = class(TXMLNode, IXMLNotprsubsettype)
  protected
    { IXMLNotprsubsettype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLSetdifftype }

  TXMLSetdifftype = class(TXMLNode, IXMLSetdifftype)
  protected
    { IXMLSetdifftype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLCardtype }

  TXMLCardtype = class(TXMLNode, IXMLCardtype)
  protected
    { IXMLCardtype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLCartesianproducttype }

  TXMLCartesianproducttype = class(TXMLNode, IXMLCartesianproducttype)
  protected
    { IXMLCartesianproducttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLRelationstype }

  TXMLRelationstype = class(TXMLNode, IXMLRelationstype)
  protected
    { IXMLRelationstype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLLogbasetype }

  TXMLLogbasetype = class(TXMLMathTextBaseType, IXMLLogbasetype)
  protected
    { IXMLLogbasetype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLInttype }

  TXMLInttype = class(TXMLNode, IXMLInttype)
  protected
    { IXMLInttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLDifftype }

  TXMLDifftype = class(TXMLNode, IXMLDifftype)
  protected
    { IXMLDifftype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLPartialdifftype }

  TXMLPartialdifftype = class(TXMLNode, IXMLPartialdifftype)
  protected
    { IXMLPartialdifftype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLLimittype }

  TXMLLimittype = class(TXMLNode, IXMLLimittype)
  protected
    { IXMLLimittype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLLowlimittype }

  TXMLLowlimittype = class(TXMLMathTextBaseType, IXMLLowlimittype)
  protected
    { IXMLLowlimittype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLUplimittype }

  TXMLUplimittype = class(TXMLMathTextBaseType, IXMLUplimittype)
  protected
    { IXMLUplimittype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTendstotype }

  TXMLTendstotype = class(TXMLNode, IXMLTendstotype)
  protected
    { IXMLTendstotype }
    function Get_Type_: UnicodeString;
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Type_(Value: UnicodeString);
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;

{ TXMLVectortype }

  TXMLVectortype = class(TXMLMathTextBaseType, IXMLVectortype)
  protected
    { IXMLVectortype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;

    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMatrixtype }

  TXMLMatrixtype = class(TXMLNode, IXMLMatrixtype)
  protected
    { IXMLMatrixtype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Matrixrow: IXMLMatrixrowtype;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMatrixrowtype }

  TXMLMatrixrowtype = class(TXMLNode, IXMLMatrixrowtype)
  protected
    { IXMLMatrixrowtype }
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    function Get_Cn: IXMLCntype;
    function Get_Ci: IXMLCitype;
    function Get_Csymbol: IXMLCsymboltype;
    function Get_Abs: IXMLArithtype;
    function Get_Conjugate: IXMLArithtype;
    function Get_Factorial: IXMLArithtype;
    function Get_Arg: IXMLArithtype;
    function Get_Real: IXMLArithtype;
    function Get_Imaginary: IXMLArithtype;
    function Get_Floor: IXMLArithtype;
    function Get_Ceiling: IXMLArithtype;
    function Get_Quotient: IXMLArithtype;
    function Get_Divide: IXMLArithtype;
    function Get_Rem: IXMLArithtype;
    function Get_Minus: IXMLArithtype;
    function Get_Plus: IXMLArithtype;
    function Get_Times: IXMLArithtype;
    function Get_Power: IXMLArithtype;
    function Get_Root: IXMLArithtype;
    function Get_Max: IXMLArithtype;
    function Get_Min: IXMLArithtype;
    function Get_Gcd: IXMLArithtype;
    function Get_Lcm: IXMLArithtype;
    function Get_Sum: IXMLArithtype;
    function Get_Product: IXMLArithtype;
    function Get_Compose: IXMLFunctionstype;
    function Get_Domain: IXMLFunctionstype;
    function Get_Codomain: IXMLFunctionstype;
    function Get_Image: IXMLFunctionstype;
    function Get_Domainofapplication: IXMLDomainofapplicationtype;
    function Get_Ident: IXMLFunctionstype;
    function Get_And_: IXMLElementaryfunctionstype;
    function Get_Or_: IXMLLogictype;
    function Get_Xor_: IXMLLogictype;
    function Get_Not_: IXMLLogictype;
    function Get_Exists: IXMLLogictype;
    function Get_Forall: IXMLLogictype;
    function Get_Implies: IXMLLogictype;
    function Get_Naturalnumbers: IXMLConstanttype;
    function Get_Primes: IXMLConstanttype;
    function Get_Integers: IXMLConstanttype;
    function Get_Rationals: IXMLConstanttype;
    function Get_Reals: IXMLConstanttype;
    function Get_Complexes: IXMLConstanttype;
    function Get_Emptyset: IXMLConstanttype;
    function Get_Exponentiale: IXMLConstanttype;
    function Get_Imaginaryi: IXMLConstanttype;
    function Get_Pi: IXMLConstanttype;
    function Get_Eulergamma: IXMLConstanttype;
    function Get_True: IXMLConstanttype;
    function Get_False: IXMLConstanttype;
    function Get_Infinity: IXMLConstanttype;
    function Get_Notanumber: IXMLConstanttype;
    function Get_Set_: IXMLSettype;
    function Get_List: IXMLListtype;
    function Get_Union: IXMLUniontype;
    function Get_Intersect: IXMLIntersecttype;
    function Get_In_: IXMLIntype;
    function Get_Notin: IXMLNotintype;
    function Get_Subset: IXMLSubsettype;
    function Get_Prsubset: IXMLPrsubsettype;
    function Get_Notsubset: IXMLNotsubsettype;
    function Get_Notprsubset: IXMLNotprsubsettype;
    function Get_Setdiff: IXMLSetdifftype;
    function Get_Card: IXMLCardtype;
    function Get_Cartesianproduct: IXMLCartesianproducttype;
    function Get_Eq: IXMLRelationstype;
    function Get_Neq: IXMLRelationstype;
    function Get_Leq: IXMLRelationstype;
    function Get_Lt: IXMLRelationstype;
    function Get_Geq: IXMLRelationstype;
    function Get_Gt: IXMLRelationstype;
    function Get_Equivalent: IXMLRelationstype;
    function Get_Approx: IXMLRelationstype;
    function Get_Factorof: IXMLRelationstype;
    function Get_Exp: IXMLElementaryfunctionstype;
    function Get_Ln: IXMLElementaryfunctionstype;
    function Get_Log: IXMLElementaryfunctionstype;
    function Get_Logbase: IXMLLogbasetype;
    function Get_Sin: IXMLElementaryfunctionstype;
    function Get_Cos: IXMLElementaryfunctionstype;
    function Get_Tan: IXMLElementaryfunctionstype;
    function Get_Sec: IXMLElementaryfunctionstype;
    function Get_Csc: IXMLElementaryfunctionstype;
    function Get_Cot: IXMLElementaryfunctionstype;
    function Get_Arcsin: IXMLElementaryfunctionstype;
    function Get_Arccos: IXMLElementaryfunctionstype;
    function Get_Arctan: IXMLElementaryfunctionstype;
    function Get_Arcsec: IXMLElementaryfunctionstype;
    function Get_Arccsc: IXMLElementaryfunctionstype;
    function Get_Arccot: IXMLElementaryfunctionstype;
    function Get_Sinh: IXMLElementaryfunctionstype;
    function Get_Cosh: IXMLElementaryfunctionstype;
    function Get_Tanh: IXMLElementaryfunctionstype;
    function Get_Sech: IXMLElementaryfunctionstype;
    function Get_Csch: IXMLElementaryfunctionstype;
    function Get_Coth: IXMLElementaryfunctionstype;
    function Get_Arccosh: IXMLElementaryfunctionstype;
    function Get_Arccoth: IXMLElementaryfunctionstype;
    function Get_Arccsch: IXMLElementaryfunctionstype;
    function Get_Arcsech: IXMLElementaryfunctionstype;
    function Get_Arcsinh: IXMLElementaryfunctionstype;
    function Get_Arctanh: IXMLElementaryfunctionstype;
    function Get_Int: IXMLInttype;
    function Get_Diff: IXMLDifftype;
    function Get_Partialdiff: IXMLPartialdifftype;
    function Get_Limit: IXMLLimittype;
    function Get_Lowlimit: IXMLLowlimittype;
    function Get_Uplimit: IXMLUplimittype;
    function Get_Tendsto: IXMLTendstotype;
    function Get_Vector: IXMLVectortype;
    function Get_Matrix: IXMLMatrixtype;
    function Get_Determinant: IXMLDeterminanttype;
    function Get_Transpose: IXMLTransposetype;
    function Get_Selector: IXMLSelectortype;
    function Get_Vectorproduct: IXMLVectorproducttype;
    function Get_Scalarproduct: IXMLScalarproducttype;
    function Get_Outerproduct: IXMLOuterproducttype;
    function Get_Divergence: IXMLDivergencetype;
    function Get_Grad: IXMLGradtype;
    function Get_Curl: IXMLCurltype;
    function Get_Laplacian: IXMLLaplaciantype;
    function Get_Mean: IXMLMeantype;
    function Get_Sdev: IXMLSdevtype;
    function Get_Variance: IXMLVariancetype;
    function Get_Median: IXMLMediantype;
    function Get_Mode: IXMLModetype;
    function Get_Moment: IXMLMomenttype;
    function Get_Momentabout: IXMLMomentabouttype;
    function Get_Apply: IXMLApplytype;
    function Get_Interval: IXMLIntervaltype;
    function Get_Inverse: IXMLInversetype;
    function Get_Condition: IXMLConditiontype;
    function Get_Declare: IXMLDeclaretype;
    function Get_Lambda: IXMLLambdatype;
    function Get_Piecewise: IXMLPiecewisetype;
    function Get_Bvar: IXMLBvartype;
    function Get_Degree: IXMLDegreetype;
    function Get_Semantics: IXMLSemanticstype;
    function Get_Mi: IXMLMitype;
    function Get_Mo: IXMLMotype;
    function Get_Mn: IXMLMntype;
    function Get_Mtext: IXMLMtexttype;
    function Get_Ms: IXMLMstype;
    function Get_Mrow: IXMLMrowtype;
    function Get_Mfrac: IXMLMfractype;
    function Get_Msqrt: IXMLMsqrttype;
    function Get_Mroot: IXMLMroottype;
    function Get_Mpadded: IXMLMpaddedtype;
    function Get_Mphantom: IXMLMphantomtype;
    function Get_Mfenced: IXMLMfencedtype;
    function Get_Menclose: IXMLMenclosetype;
    function Get_Msub: IXMLMsubtype;
    function Get_Msup: IXMLMsuptype;
    function Get_Msubsup: IXMLMsubsuptype;
    function Get_Munder: IXMLMundertype;
    function Get_Mover: IXMLMovertype;
    function Get_Munderover: IXMLMunderovertype;
    function Get_Mmultiscripts: IXMLMmultiscriptstype;
    function Get_Mtable: IXMLMtabletype;
    function Get_Maligngroup: IXMLMaligngrouptype;
    function Get_Malignmark: IXMLMalignmarktype;
    function Get_Mspace: IXMLMspacetype;
    function Get_Maction: IXMLMactiontype;
    function Get_Merror: IXMLMerrortype;
    function Get_Mstyle: IXMLMstyletype;
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDeterminanttype }

  TXMLDeterminanttype = class(TXMLNode, IXMLDeterminanttype)
  protected
    { IXMLDeterminanttype }
    function Get_Encoding: UnicodeString;
    function Get_DefinitionURL: UnicodeString;
    function Get_Class_: UnicodeString;
    function Get_Style: UnicodeString;
    function Get_Xref: UnicodeString;
    function Get_Id: UnicodeString;
    function Get_Href: UnicodeString;
    procedure Set_Encoding(Value: UnicodeString);
    procedure Set_DefinitionURL(Value: UnicodeString);
    procedure Set_Class_(Value: UnicodeString);
    procedure Set_Style(Value: UnicodeString);
    procedure Set_Xref(Value: UnicodeString);
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Href(Value: UnicodeString);
  end;




{ TXMLMathtype }

procedure TXMLMathtype.AfterConstruction;
begin
  inherited;
end;

function TXMLMathtype.Get_Baseline: UnicodeString;
begin
  Result := AttributeNodes['baseline'].Text;
end;

procedure TXMLMathtype.Set_Baseline(Value: UnicodeString);
begin
  SetAttribute('baseline', Value);
end;

function TXMLMathtype.Get_Overflow: UnicodeString;
begin
  Result := AttributeNodes['overflow'].Text;
end;

procedure TXMLMathtype.Set_Overflow(Value: UnicodeString);
begin
  SetAttribute('overflow', Value);
end;

function TXMLMathtype.Get_Altimg: UnicodeString;
begin
  Result := AttributeNodes['altimg'].Text;
end;

procedure TXMLMathtype.Set_Altimg(Value: UnicodeString);
begin
  SetAttribute('altimg', Value);
end;

function TXMLMathtype.Get_Alttext: UnicodeString;
begin
  Result := AttributeNodes['alttext'].Text;
end;

procedure TXMLMathtype.Set_Alttext(Value: UnicodeString);
begin
  SetAttribute('alttext', Value);
end;

function TXMLMathtype.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLMathtype.Set_Type_(Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLMathtype.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLMathtype.Set_Name(Value: UnicodeString);
begin
  SetAttribute('name', Value);
end;

function TXMLMathtype.Get_Height: UnicodeString;
begin
  Result := AttributeNodes['height'].Text;
end;

procedure TXMLMathtype.Set_Height(Value: UnicodeString);
begin
  SetAttribute('height', Value);
end;

function TXMLMathtype.Get_Width: UnicodeString;
begin
  Result := AttributeNodes['width'].Text;
end;

procedure TXMLMathtype.Set_Width(Value: UnicodeString);
begin
  SetAttribute('width', Value);
end;

function TXMLMathtype.Get_Macros: UnicodeString;
begin
  Result := AttributeNodes['macros'].Text;
end;

procedure TXMLMathtype.Set_Macros(Value: UnicodeString);
begin
  SetAttribute('macros', Value);
end;

function TXMLMathtype.Get_Display: UnicodeString;
begin
  Result := AttributeNodes['display'].Text;
end;

procedure TXMLMathtype.Set_Display(Value: UnicodeString);
begin
  SetAttribute('display', Value);
end;

function TXMLMathtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMathtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMathtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMathtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMathtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMathtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMathtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMathtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMathtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMathtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLConditiontype }

procedure TXMLConditiontype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLConditiontype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLConditiontype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLConditiontype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLConditiontype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLConditiontype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLConditiontype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLConditiontype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLConditiontype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLConditiontype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLConditiontype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLConditiontype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLConditiontype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLConditiontype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLConditiontype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLConditiontype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLConditiontype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLConditiontype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLConditiontype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLConditiontype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLConditiontype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLConditiontype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLConditiontype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLConditiontype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLConditiontype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLConditiontype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLConditiontype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLConditiontype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLConditiontype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLConditiontype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLConditiontype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLConditiontype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLConditiontype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLConditiontype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLConditiontype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLConditiontype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLConditiontype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLConditiontype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLConditiontype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLConditiontype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLConditiontype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLConditiontype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLConditiontype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLConditiontype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLConditiontype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLConditiontype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLConditiontype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLConditiontype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLConditiontype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLConditiontype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLConditiontype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLConditiontype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLConditiontype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLConditiontype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLConditiontype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLConditiontype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLConditiontype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLConditiontype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLConditiontype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLConditiontype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLConditiontype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLConditiontype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLConditiontype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLConditiontype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLConditiontype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLConditiontype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLConditiontype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLConditiontype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLConditiontype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLConditiontype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLConditiontype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLConditiontype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLConditiontype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLConditiontype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLDeclaretype }

procedure TXMLDeclaretype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLDeclaretype.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLDeclaretype.Set_Type_(Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLDeclaretype.Get_Scope: UnicodeString;
begin
  Result := AttributeNodes['scope'].Text;
end;

procedure TXMLDeclaretype.Set_Scope(Value: UnicodeString);
begin
  SetAttribute('scope', Value);
end;

function TXMLDeclaretype.Get_Nargs: LongWord;
begin
  Result := AttributeNodes['nargs'].NodeValue;
end;

procedure TXMLDeclaretype.Set_Nargs(Value: LongWord);
begin
  SetAttribute('nargs', Value);
end;

function TXMLDeclaretype.Get_Occurrence: UnicodeString;
begin
  Result := AttributeNodes['occurrence'].Text;
end;

procedure TXMLDeclaretype.Set_Occurrence(Value: UnicodeString);
begin
  SetAttribute('occurrence', Value);
end;

function TXMLDeclaretype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLDeclaretype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLDeclaretype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLDeclaretype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLDeclaretype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLDeclaretype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLDeclaretype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLDeclaretype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLDeclaretype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLDeclaretype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLDeclaretype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLDeclaretype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLDeclaretype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLDeclaretype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLDeclaretype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLDeclaretype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLDeclaretype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLDeclaretype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLDeclaretype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLDeclaretype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLDeclaretype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLDeclaretype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLDeclaretype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLDeclaretype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLDeclaretype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLDeclaretype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLDeclaretype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLDeclaretype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLDeclaretype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLDeclaretype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLDeclaretype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLDeclaretype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLDeclaretype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLDeclaretype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLDeclaretype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLDeclaretype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLDeclaretype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLDeclaretype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLDeclaretype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLDeclaretype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLDeclaretype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLDeclaretype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLDeclaretype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLDeclaretype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLDeclaretype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLDeclaretype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLDeclaretype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLDeclaretype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLDeclaretype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLDeclaretype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLDeclaretype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLDeclaretype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLDeclaretype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLDeclaretype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLDeclaretype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLDeclaretype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLDeclaretype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLDeclaretype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLDeclaretype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLDeclaretype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLDeclaretype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLDeclaretype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLDeclaretype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLDeclaretype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLDeclaretype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLDeclaretype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLDeclaretype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLDeclaretype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLDeclaretype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLDeclaretype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLDeclaretype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLDeclaretype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLDeclaretype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLLambdatype }

procedure TXMLLambdatype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLLambdatype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLLambdatype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLLambdatype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLLambdatype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLLambdatype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLLambdatype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLLambdatype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLLambdatype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLLambdatype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLLambdatype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLLambdatype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLLambdatype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLLambdatype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLLambdatype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLLambdatype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLLambdatype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLLambdatype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLLambdatype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLLambdatype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLLambdatype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLLambdatype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLLambdatype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLLambdatype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLLambdatype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLLambdatype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLLambdatype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLLambdatype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLLambdatype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLLambdatype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLLambdatype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLLambdatype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLLambdatype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLLambdatype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLLambdatype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLLambdatype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLLambdatype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLLambdatype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLLambdatype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLLambdatype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLLambdatype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLLambdatype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLLambdatype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLLambdatype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLLambdatype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLLambdatype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLLambdatype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLLambdatype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLLambdatype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLLambdatype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLLambdatype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLLambdatype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLLambdatype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLLambdatype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLLambdatype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLLambdatype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLLambdatype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLLambdatype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLLambdatype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLLambdatype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLLambdatype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLLambdatype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLLambdatype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLLambdatype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLLambdatype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLLambdatype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLLambdatype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLLambdatype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLLambdatype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLLambdatype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLLambdatype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLLambdatype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLLambdatype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLLambdatype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLLambdatype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLLambdatype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLLambdatype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLLambdatype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLLambdatype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLLambdatype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLPiecewisetype }

procedure TXMLPiecewisetype.AfterConstruction;
begin
  RegisterChildNode('piece', TXMLPiecetype);
  RegisterChildNode('otherwise', TXMLOtherwisetype);
  FPiece := CreateCollection(TXMLPiecetypeList, IXMLPiecetype, 'piece') as IXMLPiecetypeList;
  inherited;
end;

function TXMLPiecewisetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLPiecewisetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLPiecewisetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLPiecewisetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLPiecewisetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLPiecewisetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLPiecewisetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLPiecewisetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLPiecewisetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLPiecewisetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLPiecewisetype.Get_Piece: IXMLPiecetypeList;
begin
  Result := FPiece;
end;

function TXMLPiecewisetype.Get_Otherwise: IXMLOtherwisetype;
begin
  Result := ChildNodes['otherwise'] as IXMLOtherwisetype;
end;


{ TXMLMfractype }

procedure TXMLMfractype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
end;

function TXMLMfractype.Get_Bevelled: Boolean;
begin
  Result := AttributeNodes['bevelled'].NodeValue;
end;

procedure TXMLMfractype.Set_Bevelled(Value: Boolean);
begin
  SetAttribute('bevelled', Value);
end;

function TXMLMfractype.Get_Denomalign: UnicodeString;
begin
  Result := AttributeNodes['denomalign'].Text;
end;

procedure TXMLMfractype.Set_Denomalign(Value: UnicodeString);
begin
  SetAttribute('denomalign', Value);
end;

function TXMLMfractype.Get_Numalign: UnicodeString;
begin
  Result := AttributeNodes['numalign'].Text;
end;

procedure TXMLMfractype.Set_Numalign(Value: UnicodeString);
begin
  SetAttribute('numalign', Value);
end;

function TXMLMfractype.Get_Linethickness: UnicodeString;
begin
  Result := AttributeNodes['linethickness'].Text;
end;

procedure TXMLMfractype.Set_Linethickness(Value: UnicodeString);
begin
  SetAttribute('linethickness', Value);
end;

function TXMLMfractype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMfractype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMfractype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMfractype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMfractype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMfractype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMfractype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMfractype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMfractype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMfractype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMsqrttype }

procedure TXMLMsqrttype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);

end;

function TXMLMsqrttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMsqrttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMsqrttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMsqrttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMsqrttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMsqrttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMsqrttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMsqrttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMsqrttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMsqrttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMsubtype }

procedure TXMLMsubtype.AfterConstruction;
begin
  inherited;
end;

function TXMLMsubtype.Get_Subscriptshift: UnicodeString;
begin
  Result := AttributeNodes['subscriptshift'].Text;
end;

procedure TXMLMsubtype.Set_Subscriptshift(Value: UnicodeString);
begin
  SetAttribute('subscriptshift', Value);
end;

function TXMLMsubtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMsubtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMsubtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMsubtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMsubtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMsubtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMsubtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMsubtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMsubtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMsubtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMsuptype }

procedure TXMLMsuptype.AfterConstruction;
begin
  inherited;
end;

function TXMLMsuptype.Get_Superscriptshift: UnicodeString;
begin
  Result := AttributeNodes['superscriptshift'].Text;
end;

procedure TXMLMsuptype.Set_Superscriptshift(Value: UnicodeString);
begin
  SetAttribute('superscriptshift', Value);
end;

function TXMLMsuptype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMsuptype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMsuptype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMsuptype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMsuptype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMsuptype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMsuptype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMsuptype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMsuptype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMsuptype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMsubsuptype }

procedure TXMLMsubsuptype.AfterConstruction;
begin
  inherited;
end;

function TXMLMsubsuptype.Get_Subscriptshift: UnicodeString;
begin
  Result := AttributeNodes['subscriptshift'].Text;
end;

procedure TXMLMsubsuptype.Set_Subscriptshift(Value: UnicodeString);
begin
  SetAttribute('subscriptshift', Value);
end;

function TXMLMsubsuptype.Get_Superscriptshift: UnicodeString;
begin
  Result := AttributeNodes['superscriptshift'].Text;
end;

procedure TXMLMsubsuptype.Set_Superscriptshift(Value: UnicodeString);
begin
  SetAttribute('superscriptshift', Value);
end;

function TXMLMsubsuptype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMsubsuptype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMsubsuptype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMsubsuptype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMsubsuptype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMsubsuptype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMsubsuptype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMsubsuptype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMsubsuptype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMsubsuptype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMundertype }

procedure TXMLMundertype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
end;

function TXMLMundertype.Get_Accentunder: Boolean;
begin
  Result := AttributeNodes['accentunder'].NodeValue;
end;

procedure TXMLMundertype.Set_Accentunder(Value: Boolean);
begin
  SetAttribute('accentunder', Value);
end;

function TXMLMundertype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMundertype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMundertype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMundertype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMundertype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMundertype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMundertype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMundertype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMundertype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMundertype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMovertype }

procedure TXMLMovertype.AfterConstruction;
begin
  inherited;
end;

function TXMLMovertype.Get_Accent: Boolean;
begin
  Result := AttributeNodes['accent'].NodeValue;
end;

procedure TXMLMovertype.Set_Accent(Value: Boolean);
begin
  SetAttribute('accent', Value);
end;

function TXMLMovertype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMovertype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMovertype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMovertype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMovertype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMovertype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMovertype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMovertype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMovertype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMovertype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMunderovertype }

procedure TXMLMunderovertype.AfterConstruction;
begin
  inherited;
end;

function TXMLMunderovertype.Get_Accent: Boolean;
begin
  Result := AttributeNodes['accent'].NodeValue;
end;

procedure TXMLMunderovertype.Set_Accent(Value: Boolean);
begin
  SetAttribute('accent', Value);
end;

function TXMLMunderovertype.Get_Accentunder: Boolean;
begin
  Result := AttributeNodes['accentunder'].NodeValue;
end;

procedure TXMLMunderovertype.Set_Accentunder(Value: Boolean);
begin
  SetAttribute('accentunder', Value);
end;

function TXMLMunderovertype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMunderovertype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMunderovertype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMunderovertype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMunderovertype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMunderovertype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMunderovertype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMunderovertype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMunderovertype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMunderovertype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMmultiscriptstype }

procedure TXMLMmultiscriptstype.AfterConstruction;
begin
  RegisterChildNode('none', TXMLNonetype);
  RegisterChildNode('mprescripts', TXMLMprescriptstype);
  inherited;
end;

function TXMLMmultiscriptstype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMmultiscriptstype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMmultiscriptstype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMmultiscriptstype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMmultiscriptstype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMmultiscriptstype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMmultiscriptstype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMmultiscriptstype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMmultiscriptstype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMmultiscriptstype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


function TXMLMmultiscriptstype.Get_None: IXMLNonetype;
begin
  Result := ChildNodes['none'] as IXMLNonetype;
end;

function TXMLMmultiscriptstype.Get_Mprescripts: IXMLMprescriptstype;
begin
  Result := ChildNodes['mprescripts'] as IXMLMprescriptstype;
end;
{ TXMLMtdtype }

procedure TXMLMtdtype.AfterConstruction;
begin
  inherited;
end;

function TXMLMtdtype.Get_Rowalign: UnicodeString;
begin
  Result := AttributeNodes['rowalign'].Text;
end;

procedure TXMLMtdtype.Set_Rowalign(Value: UnicodeString);
begin
  SetAttribute('rowalign', Value);
end;

function TXMLMtdtype.Get_Columnalign: UnicodeString;
begin
  Result := AttributeNodes['columnalign'].Text;
end;

procedure TXMLMtdtype.Set_Columnalign(Value: UnicodeString);
begin
  SetAttribute('columnalign', Value);
end;

function TXMLMtdtype.Get_Groupalign: UnicodeString;
begin
  Result := AttributeNodes['groupalign'].Text;
end;

procedure TXMLMtdtype.Set_Groupalign(Value: UnicodeString);
begin
  SetAttribute('groupalign', Value);
end;

function TXMLMtdtype.Get_Columnspan: LongWord;
begin
  Result := AttributeNodes['columnspan'].NodeValue;
end;

procedure TXMLMtdtype.Set_Columnspan(Value: LongWord);
begin
  SetAttribute('columnspan', Value);
end;

function TXMLMtdtype.Get_Rowspan: LongWord;
begin
  Result := AttributeNodes['rowspan'].NodeValue;
end;

procedure TXMLMtdtype.Set_Rowspan(Value: LongWord);
begin
  SetAttribute('rowspan', Value);
end;

function TXMLMtdtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMtdtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMtdtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMtdtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMtdtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMtdtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMtdtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMtdtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMtdtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMtdtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMaligngrouptype }

function TXMLMaligngrouptype.Get_Groupalign: UnicodeString;
begin
  Result := AttributeNodes['groupalign'].Text;
end;

procedure TXMLMaligngrouptype.Set_Groupalign(Value: UnicodeString);
begin
  SetAttribute('groupalign', Value);
end;

function TXMLMaligngrouptype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMaligngrouptype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMaligngrouptype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMaligngrouptype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMaligngrouptype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMaligngrouptype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMaligngrouptype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMaligngrouptype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMaligngrouptype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMaligngrouptype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMspacetype }

function TXMLMspacetype.Get_Width: UnicodeString;
begin
  Result := AttributeNodes['width'].Text;
end;

procedure TXMLMspacetype.Set_Width(Value: UnicodeString);
begin
  SetAttribute('width', Value);
end;

function TXMLMspacetype.Get_Height: UnicodeString;
begin
  Result := AttributeNodes['height'].Text;
end;

procedure TXMLMspacetype.Set_Height(Value: UnicodeString);
begin
  SetAttribute('height', Value);
end;

function TXMLMspacetype.Get_Depth: UnicodeString;
begin
  Result := AttributeNodes['depth'].Text;
end;

procedure TXMLMspacetype.Set_Depth(Value: UnicodeString);
begin
  SetAttribute('depth', Value);
end;

function TXMLMspacetype.Get_Linebreak: UnicodeString;
begin
  Result := AttributeNodes['linebreak'].Text;
end;

procedure TXMLMspacetype.Set_Linebreak(Value: UnicodeString);
begin
  SetAttribute('linebreak', Value);
end;

function TXMLMspacetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMspacetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMspacetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMspacetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMspacetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMspacetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMspacetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMspacetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMspacetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMspacetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMactiontype }

procedure TXMLMactiontype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
end;

function TXMLMactiontype.Get_Actiontype: UnicodeString;
begin
  Result := AttributeNodes['actiontype'].Text;
end;

procedure TXMLMactiontype.Set_Actiontype(Value: UnicodeString);
begin
  SetAttribute('actiontype', Value);
end;

function TXMLMactiontype.Get_Selection: LongWord;
begin
  Result := AttributeNodes['selection'].NodeValue;
end;

procedure TXMLMactiontype.Set_Selection(Value: LongWord);
begin
  SetAttribute('selection', Value);
end;

function TXMLMactiontype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMactiontype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMactiontype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMactiontype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMactiontype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMactiontype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMactiontype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMactiontype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMactiontype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMactiontype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMerrortype }

procedure TXMLMerrortype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
end;

function TXMLMerrortype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMerrortype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMerrortype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMerrortype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMerrortype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMerrortype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMerrortype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMerrortype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMerrortype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMerrortype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMstyletype }

procedure TXMLMstyletype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
end;

function TXMLMstyletype.Get_Scriptlevel: Integer;
begin
  Result := AttributeNodes['scriptlevel'].NodeValue;
end;

procedure TXMLMstyletype.Set_Scriptlevel(Value: Integer);
begin
  SetAttribute('scriptlevel', Value);
end;

function TXMLMstyletype.Get_Displaystyle: Boolean;
begin
  Result := AttributeNodes['displaystyle'].NodeValue;
end;

procedure TXMLMstyletype.Set_Displaystyle(Value: Boolean);
begin
  SetAttribute('displaystyle', Value);
end;

function TXMLMstyletype.Get_Scriptsizemultiplier: UnicodeString;
begin
  Result := AttributeNodes['scriptsizemultiplier'].Text;
end;

procedure TXMLMstyletype.Set_Scriptsizemultiplier(Value: UnicodeString);
begin
  SetAttribute('scriptsizemultiplier', Value);
end;

function TXMLMstyletype.Get_Scriptminsize: UnicodeString;
begin
  Result := AttributeNodes['scriptminsize'].Text;
end;

procedure TXMLMstyletype.Set_Scriptminsize(Value: UnicodeString);
begin
  SetAttribute('scriptminsize', Value);
end;

function TXMLMstyletype.Get_Color: UnicodeString;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLMstyletype.Set_Color(Value: UnicodeString);
begin
  SetAttribute('color', Value);
end;

function TXMLMstyletype.Get_Background: UnicodeString;
begin
  Result := AttributeNodes['background'].Text;
end;

procedure TXMLMstyletype.Set_Background(Value: UnicodeString);
begin
  SetAttribute('background', Value);
end;

function TXMLMstyletype.Get_Veryverythinmathspace: UnicodeString;
begin
  Result := AttributeNodes['veryverythinmathspace'].Text;
end;

procedure TXMLMstyletype.Set_Veryverythinmathspace(Value: UnicodeString);
begin
  SetAttribute('veryverythinmathspace', Value);
end;

function TXMLMstyletype.Get_Verythinmathspace: UnicodeString;
begin
  Result := AttributeNodes['verythinmathspace'].Text;
end;

procedure TXMLMstyletype.Set_Verythinmathspace(Value: UnicodeString);
begin
  SetAttribute('verythinmathspace', Value);
end;

function TXMLMstyletype.Get_Thinmathspace: UnicodeString;
begin
  Result := AttributeNodes['thinmathspace'].Text;
end;

procedure TXMLMstyletype.Set_Thinmathspace(Value: UnicodeString);
begin
  SetAttribute('thinmathspace', Value);
end;

function TXMLMstyletype.Get_Mediummathspace: UnicodeString;
begin
  Result := AttributeNodes['mediummathspace'].Text;
end;

procedure TXMLMstyletype.Set_Mediummathspace(Value: UnicodeString);
begin
  SetAttribute('mediummathspace', Value);
end;

function TXMLMstyletype.Get_Thickmathspace: UnicodeString;
begin
  Result := AttributeNodes['thickmathspace'].Text;
end;

procedure TXMLMstyletype.Set_Thickmathspace(Value: UnicodeString);
begin
  SetAttribute('thickmathspace', Value);
end;

function TXMLMstyletype.Get_Verythickmathspace: UnicodeString;
begin
  Result := AttributeNodes['verythickmathspace'].Text;
end;

procedure TXMLMstyletype.Set_Verythickmathspace(Value: UnicodeString);
begin
  SetAttribute('verythickmathspace', Value);
end;

function TXMLMstyletype.Get_Veryverythickmathspace: UnicodeString;
begin
  Result := AttributeNodes['veryverythickmathspace'].Text;
end;

procedure TXMLMstyletype.Set_Veryverythickmathspace(Value: UnicodeString);
begin
  SetAttribute('veryverythickmathspace', Value);
end;

function TXMLMstyletype.Get_Linethickness: UnicodeString;
begin
  Result := AttributeNodes['linethickness'].Text;
end;

procedure TXMLMstyletype.Set_Linethickness(Value: UnicodeString);
begin
  SetAttribute('linethickness', Value);
end;

function TXMLMstyletype.Get_Form: UnicodeString;
begin
  Result := AttributeNodes['form'].Text;
end;

procedure TXMLMstyletype.Set_Form(Value: UnicodeString);
begin
  SetAttribute('form', Value);
end;

function TXMLMstyletype.Get_Lspace: UnicodeString;
begin
  Result := AttributeNodes['lspace'].Text;
end;

procedure TXMLMstyletype.Set_Lspace(Value: UnicodeString);
begin
  SetAttribute('lspace', Value);
end;

function TXMLMstyletype.Get_Rspace: UnicodeString;
begin
  Result := AttributeNodes['rspace'].Text;
end;

procedure TXMLMstyletype.Set_Rspace(Value: UnicodeString);
begin
  SetAttribute('rspace', Value);
end;

function TXMLMstyletype.Get_Fence: Boolean;
begin
  Result := AttributeNodes['fence'].NodeValue;
end;

procedure TXMLMstyletype.Set_Fence(Value: Boolean);
begin
  SetAttribute('fence', Value);
end;

function TXMLMstyletype.Get_Separator: Boolean;
begin
  Result := AttributeNodes['separator'].NodeValue;
end;

procedure TXMLMstyletype.Set_Separator(Value: Boolean);
begin
  SetAttribute('separator', Value);
end;

function TXMLMstyletype.Get_Stretchy: Boolean;
begin
  Result := AttributeNodes['stretchy'].NodeValue;
end;

procedure TXMLMstyletype.Set_Stretchy(Value: Boolean);
begin
  SetAttribute('stretchy', Value);
end;

function TXMLMstyletype.Get_Symmetric: Boolean;
begin
  Result := AttributeNodes['symmetric'].NodeValue;
end;

procedure TXMLMstyletype.Set_Symmetric(Value: Boolean);
begin
  SetAttribute('symmetric', Value);
end;

function TXMLMstyletype.Get_Movablelimits: Boolean;
begin
  Result := AttributeNodes['movablelimits'].NodeValue;
end;

procedure TXMLMstyletype.Set_Movablelimits(Value: Boolean);
begin
  SetAttribute('movablelimits', Value);
end;

function TXMLMstyletype.Get_Accent: Boolean;
begin
  Result := AttributeNodes['accent'].NodeValue;
end;

procedure TXMLMstyletype.Set_Accent(Value: Boolean);
begin
  SetAttribute('accent', Value);
end;

function TXMLMstyletype.Get_Largeop: Boolean;
begin
  Result := AttributeNodes['largeop'].NodeValue;
end;

procedure TXMLMstyletype.Set_Largeop(Value: Boolean);
begin
  SetAttribute('largeop', Value);
end;

function TXMLMstyletype.Get_Minsize: UnicodeString;
begin
  Result := AttributeNodes['minsize'].Text;
end;

procedure TXMLMstyletype.Set_Minsize(Value: UnicodeString);
begin
  SetAttribute('minsize', Value);
end;

function TXMLMstyletype.Get_Maxsize: UnicodeString;
begin
  Result := AttributeNodes['maxsize'].Text;
end;

procedure TXMLMstyletype.Set_Maxsize(Value: UnicodeString);
begin
  SetAttribute('maxsize', Value);
end;

function TXMLMstyletype.Get_Mathvariant: UnicodeString;
begin
  Result := AttributeNodes['mathvariant'].Text;
end;

procedure TXMLMstyletype.Set_Mathvariant(Value: UnicodeString);
begin
  SetAttribute('mathvariant', Value);
end;

function TXMLMstyletype.Get_Mathsize: UnicodeString;
begin
  Result := AttributeNodes['mathsize'].Text;
end;

procedure TXMLMstyletype.Set_Mathsize(Value: UnicodeString);
begin
  SetAttribute('mathsize', Value);
end;

function TXMLMstyletype.Get_Mathcolor: UnicodeString;
begin
  Result := AttributeNodes['mathcolor'].Text;
end;

procedure TXMLMstyletype.Set_Mathcolor(Value: UnicodeString);
begin
  SetAttribute('mathcolor', Value);
end;

function TXMLMstyletype.Get_Mathbackground: UnicodeString;
begin
  Result := AttributeNodes['mathbackground'].Text;
end;

procedure TXMLMstyletype.Set_Mathbackground(Value: UnicodeString);
begin
  SetAttribute('mathbackground', Value);
end;

function TXMLMstyletype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMstyletype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMstyletype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMstyletype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMstyletype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMstyletype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMstyletype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMstyletype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMstyletype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMstyletype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLCntype }

procedure TXMLCntype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('sep', TXMLSeptype);
end;

function TXMLCntype.Get_Base: LongWord;
begin
  Result := AttributeNodes['base'].NodeValue;
end;

procedure TXMLCntype.Set_Base(Value: LongWord);
begin
  SetAttribute('base', Value);
end;

function TXMLCntype.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLCntype.Set_Type_(Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLCntype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLCntype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLCntype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLCntype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLCntype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLCntype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLCntype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLCntype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLCntype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLCntype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLCntype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLCntype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLCntype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLCntype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


function TXMLCntype.Get_Sep: IXMLSeptype;
begin
  Result := ChildNodes['sep'] as IXMLSeptype;
end;

{ TXMLCitype }

procedure TXMLCitype.AfterConstruction;
begin
  inherited;
end;

function TXMLCitype.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLCitype.Set_Type_(Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLCitype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLCitype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLCitype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLCitype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLCitype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLCitype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLCitype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLCitype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLCitype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLCitype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLCitype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLCitype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLCitype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLCitype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLCsymboltype }

procedure TXMLCsymboltype.AfterConstruction;
begin
  inherited;
end;

function TXMLCsymboltype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLCsymboltype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLCsymboltype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLCsymboltype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLCsymboltype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLCsymboltype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLCsymboltype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLCsymboltype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLCsymboltype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLCsymboltype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLCsymboltype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLCsymboltype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLCsymboltype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLCsymboltype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLArithtype }

function TXMLArithtype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLArithtype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLArithtype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLArithtype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLArithtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLArithtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLArithtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLArithtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLArithtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLArithtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLArithtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLArithtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLArithtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLArithtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLFunctionstype }

function TXMLFunctionstype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLFunctionstype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLFunctionstype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLFunctionstype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLFunctionstype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLFunctionstype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLFunctionstype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLFunctionstype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLFunctionstype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLFunctionstype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLFunctionstype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLFunctionstype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLFunctionstype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLFunctionstype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLDomainofapplicationtype }

procedure TXMLDomainofapplicationtype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLDomainofapplicationtype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLDomainofapplicationtype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLDomainofapplicationtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLDomainofapplicationtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLDomainofapplicationtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLDomainofapplicationtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLDomainofapplicationtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLDomainofapplicationtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLDomainofapplicationtype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLDomainofapplicationtype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLDomainofapplicationtype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLDomainofapplicationtype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLDomainofapplicationtype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLDomainofapplicationtype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLDomainofapplicationtype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLDomainofapplicationtype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLDomainofapplicationtype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLDomainofapplicationtype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLDomainofapplicationtype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLDomainofapplicationtype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLDomainofapplicationtype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLDomainofapplicationtype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLDomainofapplicationtype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLDomainofapplicationtype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLDomainofapplicationtype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLDomainofapplicationtype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLDomainofapplicationtype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLDomainofapplicationtype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLDomainofapplicationtype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLDomainofapplicationtype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLDomainofapplicationtype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLDomainofapplicationtype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLDomainofapplicationtype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLDomainofapplicationtype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLDomainofapplicationtype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLDomainofapplicationtype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLDomainofapplicationtype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLDomainofapplicationtype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLDomainofapplicationtype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLDomainofapplicationtype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLDomainofapplicationtype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLDomainofapplicationtype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLDomainofapplicationtype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLDomainofapplicationtype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLDomainofapplicationtype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLDomainofapplicationtype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLDomainofapplicationtype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLDomainofapplicationtype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLDomainofapplicationtype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLDomainofapplicationtype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLDomainofapplicationtype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLDomainofapplicationtype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLDomainofapplicationtype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLDomainofapplicationtype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLDomainofapplicationtype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLDomainofapplicationtype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLDomainofapplicationtype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLDomainofapplicationtype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLDomainofapplicationtype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLDomainofapplicationtype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLDomainofapplicationtype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLDomainofapplicationtype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLDomainofapplicationtype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLDomainofapplicationtype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLDomainofapplicationtype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLDomainofapplicationtype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLDomainofapplicationtype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLDomainofapplicationtype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLDomainofapplicationtype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLDomainofapplicationtype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLDomainofapplicationtype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLDomainofapplicationtype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLDomainofapplicationtype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLElementaryfunctionstype }

function TXMLElementaryfunctionstype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLElementaryfunctionstype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLElementaryfunctionstype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLElementaryfunctionstype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLElementaryfunctionstype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLElementaryfunctionstype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLElementaryfunctionstype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLElementaryfunctionstype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLLogictype }

function TXMLLogictype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLLogictype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLLogictype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLLogictype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLLogictype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLLogictype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLLogictype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLLogictype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLLogictype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLLogictype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLLogictype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLLogictype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLLogictype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLLogictype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLConstanttype }

function TXMLConstanttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLConstanttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLConstanttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLConstanttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLConstanttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLConstanttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLConstanttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLConstanttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLConstanttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLConstanttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLConstanttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLConstanttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLConstanttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLConstanttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLSettype }

procedure TXMLSettype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLSettype.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLSettype.Set_Type_(Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLSettype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLSettype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLSettype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSettype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLSettype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLSettype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLSettype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSettype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLSettype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLSettype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLSettype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLSettype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLSettype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLSettype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLSettype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLSettype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLSettype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLSettype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLSettype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLSettype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLSettype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLSettype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLSettype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLSettype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLSettype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLSettype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLSettype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLSettype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLSettype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLSettype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLSettype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLSettype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLSettype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLSettype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLSettype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLSettype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLSettype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLSettype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLSettype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLSettype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLSettype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLSettype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLSettype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLSettype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLSettype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLSettype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLSettype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLSettype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLSettype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLSettype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLSettype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLSettype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLSettype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLSettype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLSettype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLSettype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLSettype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLSettype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLSettype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLSettype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLSettype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLSettype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLSettype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLSettype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLSettype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLSettype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLSettype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLSettype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLSettype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLSettype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLSettype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLSettype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLSettype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLSettype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLSettype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLSettype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLSettype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLSettype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLSettype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLSettype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLSettype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLSettype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLSettype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLSettype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLSettype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLSettype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLSettype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLSettype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLSettype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLSettype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLSettype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLSettype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLSettype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLSettype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLSettype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLSettype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLSettype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLSettype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLSettype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLSettype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLSettype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLSettype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLListtype }

procedure TXMLListtype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLListtype.Get_Order: UnicodeString;
begin
  Result := AttributeNodes['order'].Text;
end;

procedure TXMLListtype.Set_Order(Value: UnicodeString);
begin
  SetAttribute('order', Value);
end;

function TXMLListtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLListtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLListtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLListtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLListtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLListtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLListtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLListtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLListtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLListtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLListtype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLListtype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLListtype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLListtype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLListtype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLListtype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLListtype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLListtype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLListtype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLListtype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLListtype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLListtype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLListtype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLListtype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLListtype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLListtype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLListtype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLListtype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLListtype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLListtype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLListtype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLListtype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLListtype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLListtype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLListtype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLListtype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLListtype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLListtype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLListtype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLListtype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLListtype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLListtype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLListtype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLListtype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLListtype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLListtype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLListtype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLListtype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLListtype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLListtype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLListtype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLListtype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLListtype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLListtype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLListtype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLListtype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLListtype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLListtype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLListtype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLListtype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLListtype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLListtype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLListtype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLListtype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLListtype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLListtype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLListtype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLListtype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLListtype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLListtype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLListtype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLListtype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLListtype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLListtype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLListtype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLListtype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLListtype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLListtype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLListtype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLListtype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLListtype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLListtype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLListtype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLListtype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLListtype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLListtype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLListtype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLListtype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLListtype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLListtype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLListtype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLListtype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLListtype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLListtype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLListtype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLListtype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLListtype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLListtype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLListtype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLListtype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLListtype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLListtype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;

{ TXMLUniontype }

function TXMLUniontype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLUniontype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLUniontype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLUniontype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLUniontype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLUniontype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLUniontype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLUniontype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLUniontype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLUniontype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLUniontype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLUniontype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLUniontype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLUniontype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLIntersecttype }

function TXMLIntersecttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLIntersecttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLIntersecttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLIntersecttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLIntersecttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLIntersecttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLIntersecttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLIntersecttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLIntersecttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLIntersecttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLIntersecttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLIntersecttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLIntersecttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLIntersecttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLIntype }

function TXMLIntype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLIntype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLIntype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLIntype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLIntype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLIntype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLIntype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLIntype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLIntype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLIntype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLIntype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLIntype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLIntype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLIntype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLNotintype }

function TXMLNotintype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLNotintype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLNotintype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLNotintype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLNotintype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLNotintype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLNotintype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLNotintype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLNotintype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLNotintype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLNotintype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLNotintype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLNotintype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLNotintype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLSubsettype }

function TXMLSubsettype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLSubsettype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLSubsettype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLSubsettype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLSubsettype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLSubsettype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLSubsettype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSubsettype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLSubsettype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLSubsettype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLSubsettype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSubsettype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLSubsettype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLSubsettype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLPrsubsettype }

function TXMLPrsubsettype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLPrsubsettype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLPrsubsettype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLPrsubsettype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLPrsubsettype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLPrsubsettype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLPrsubsettype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLPrsubsettype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLPrsubsettype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLPrsubsettype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLPrsubsettype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLPrsubsettype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLPrsubsettype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLPrsubsettype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLNotsubsettype }

function TXMLNotsubsettype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLNotsubsettype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLNotsubsettype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLNotsubsettype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLNotsubsettype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLNotsubsettype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLNotsubsettype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLNotsubsettype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLNotsubsettype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLNotsubsettype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLNotsubsettype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLNotsubsettype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLNotsubsettype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLNotsubsettype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLNotprsubsettype }

function TXMLNotprsubsettype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLNotprsubsettype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLNotprsubsettype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLNotprsubsettype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLNotprsubsettype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLNotprsubsettype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLNotprsubsettype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLNotprsubsettype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLNotprsubsettype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLNotprsubsettype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLNotprsubsettype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLNotprsubsettype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLNotprsubsettype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLNotprsubsettype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLSetdifftype }

function TXMLSetdifftype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLSetdifftype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLSetdifftype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLSetdifftype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLSetdifftype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLSetdifftype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLSetdifftype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSetdifftype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLSetdifftype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLSetdifftype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLSetdifftype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSetdifftype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLSetdifftype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLSetdifftype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLCardtype }

function TXMLCardtype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLCardtype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLCardtype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLCardtype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLCardtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLCardtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLCardtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLCardtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLCardtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLCardtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLCardtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLCardtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLCardtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLCardtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLCartesianproducttype }

function TXMLCartesianproducttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLCartesianproducttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLCartesianproducttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLCartesianproducttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLCartesianproducttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLCartesianproducttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLCartesianproducttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLCartesianproducttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLCartesianproducttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLCartesianproducttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLCartesianproducttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLCartesianproducttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLCartesianproducttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLCartesianproducttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLRelationstype }

function TXMLRelationstype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLRelationstype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLRelationstype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLRelationstype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLRelationstype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLRelationstype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLRelationstype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLRelationstype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLRelationstype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLRelationstype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLRelationstype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLRelationstype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLRelationstype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLRelationstype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLLogbasetype }

procedure TXMLLogbasetype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLLogbasetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLLogbasetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLLogbasetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLLogbasetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLLogbasetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLLogbasetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLLogbasetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLLogbasetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLLogbasetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLLogbasetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLLogbasetype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLLogbasetype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLLogbasetype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLLogbasetype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLLogbasetype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLLogbasetype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLLogbasetype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLLogbasetype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLLogbasetype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLLogbasetype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLLogbasetype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLLogbasetype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLLogbasetype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLLogbasetype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLLogbasetype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLLogbasetype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLLogbasetype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLLogbasetype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLLogbasetype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLLogbasetype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLLogbasetype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLLogbasetype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLLogbasetype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLLogbasetype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLLogbasetype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLLogbasetype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLLogbasetype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLLogbasetype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLLogbasetype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLLogbasetype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLLogbasetype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLLogbasetype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLLogbasetype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLLogbasetype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLLogbasetype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLLogbasetype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLLogbasetype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLLogbasetype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLLogbasetype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLLogbasetype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLLogbasetype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLLogbasetype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLLogbasetype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLLogbasetype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLLogbasetype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLLogbasetype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLLogbasetype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLLogbasetype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLLogbasetype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLLogbasetype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLLogbasetype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLLogbasetype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLLogbasetype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLLogbasetype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLLogbasetype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLLogbasetype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLLogbasetype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLLogbasetype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLLogbasetype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLLogbasetype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLLogbasetype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLLogbasetype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLLogbasetype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLLogbasetype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLLogbasetype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLLogbasetype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLLogbasetype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLLogbasetype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLLogbasetype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLInttype }

function TXMLInttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLInttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLInttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLInttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLInttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLInttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLInttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLInttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLInttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLInttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLInttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLInttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLInttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLInttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLDifftype }

function TXMLDifftype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLDifftype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLDifftype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLDifftype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLDifftype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLDifftype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLDifftype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDifftype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLDifftype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLDifftype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLDifftype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDifftype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLDifftype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLDifftype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLPartialdifftype }

function TXMLPartialdifftype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLPartialdifftype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLPartialdifftype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLPartialdifftype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLPartialdifftype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLPartialdifftype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLPartialdifftype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLPartialdifftype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLPartialdifftype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLPartialdifftype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLPartialdifftype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLPartialdifftype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLPartialdifftype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLPartialdifftype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLLimittype }

function TXMLLimittype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLLimittype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLLimittype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLLimittype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLLimittype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLLimittype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLLimittype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLLimittype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLLimittype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLLimittype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLLimittype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLLimittype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLLimittype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLLimittype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLLowlimittype }

procedure TXMLLowlimittype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLLowlimittype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLLowlimittype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLLowlimittype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLLowlimittype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLLowlimittype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLLowlimittype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLLowlimittype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLLowlimittype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLLowlimittype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLLowlimittype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLLowlimittype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLLowlimittype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLLowlimittype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLLowlimittype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLLowlimittype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLLowlimittype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLLowlimittype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLLowlimittype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLLowlimittype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLLowlimittype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLLowlimittype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLLowlimittype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLLowlimittype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLLowlimittype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLLowlimittype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLLowlimittype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLLowlimittype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLLowlimittype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLLowlimittype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLLowlimittype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLLowlimittype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLLowlimittype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLLowlimittype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLLowlimittype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLLowlimittype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLLowlimittype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLLowlimittype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLLowlimittype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLLowlimittype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLLowlimittype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLLowlimittype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLLowlimittype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLLowlimittype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLLowlimittype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLLowlimittype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLLowlimittype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLLowlimittype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLLowlimittype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLLowlimittype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLLowlimittype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLLowlimittype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLLowlimittype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLLowlimittype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLLowlimittype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLLowlimittype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLLowlimittype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLLowlimittype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLLowlimittype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLLowlimittype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLLowlimittype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLLowlimittype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLLowlimittype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLLowlimittype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLLowlimittype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLLowlimittype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLLowlimittype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLLowlimittype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLLowlimittype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLLowlimittype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLLowlimittype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLLowlimittype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLLowlimittype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLLowlimittype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLLowlimittype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLLowlimittype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLLowlimittype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLLowlimittype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLLowlimittype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLLowlimittype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLLowlimittype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLLowlimittype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLLowlimittype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLLowlimittype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;

{ TXMLUplimittype }

procedure TXMLUplimittype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLUplimittype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLUplimittype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLUplimittype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLUplimittype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLUplimittype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLUplimittype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLUplimittype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLUplimittype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLUplimittype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLUplimittype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLUplimittype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLUplimittype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLUplimittype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLUplimittype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLUplimittype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLUplimittype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLUplimittype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLUplimittype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLUplimittype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLUplimittype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLUplimittype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLUplimittype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLUplimittype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLUplimittype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLUplimittype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLUplimittype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLUplimittype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLUplimittype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLUplimittype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLUplimittype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLUplimittype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLUplimittype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLUplimittype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLUplimittype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLUplimittype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLUplimittype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLUplimittype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLUplimittype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLUplimittype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLUplimittype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLUplimittype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLUplimittype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLUplimittype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLUplimittype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLUplimittype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLUplimittype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLUplimittype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLUplimittype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLUplimittype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLUplimittype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLUplimittype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLUplimittype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLUplimittype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLUplimittype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLUplimittype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLUplimittype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLUplimittype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLUplimittype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLUplimittype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLUplimittype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLUplimittype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLUplimittype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLUplimittype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLUplimittype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLUplimittype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLUplimittype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLUplimittype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLUplimittype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLUplimittype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLUplimittype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLUplimittype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLUplimittype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLUplimittype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLUplimittype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLUplimittype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLUplimittype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLUplimittype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLUplimittype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLUplimittype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLUplimittype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLUplimittype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLUplimittype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLUplimittype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLTendstotype }

function TXMLTendstotype.Get_Type_: UnicodeString;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLTendstotype.Set_Type_(Value: UnicodeString);
begin
  SetAttribute('type', Value);
end;

function TXMLTendstotype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLTendstotype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLTendstotype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLTendstotype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLTendstotype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLTendstotype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLTendstotype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLTendstotype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLTendstotype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLTendstotype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLTendstotype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLTendstotype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLTendstotype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLTendstotype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLVectortype }

procedure TXMLVectortype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLVectortype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLVectortype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLVectortype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLVectortype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLVectortype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLVectortype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLVectortype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLVectortype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLVectortype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLVectortype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLVectortype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLVectortype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLVectortype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLVectortype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLVectortype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLVectortype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLVectortype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLVectortype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLVectortype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLVectortype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLVectortype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLVectortype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLVectortype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLVectortype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLVectortype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLVectortype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLVectortype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLVectortype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLVectortype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLVectortype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLVectortype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLVectortype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLVectortype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLVectortype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLVectortype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLVectortype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLVectortype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLVectortype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLVectortype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLVectortype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLVectortype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLVectortype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLVectortype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLVectortype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLVectortype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLVectortype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLVectortype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLVectortype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLVectortype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLVectortype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLVectortype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLVectortype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLVectortype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLVectortype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLVectortype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLVectortype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLVectortype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLVectortype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLVectortype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLVectortype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLVectortype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLVectortype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLVectortype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLVectortype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLVectortype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLVectortype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLVectortype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLVectortype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLVectortype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLVectortype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLVectortype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLVectortype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLVectortype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLVectortype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLVectortype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLVectortype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLVectortype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLVectortype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLVectortype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;

{ TXMLMatrixtype }

procedure TXMLMatrixtype.AfterConstruction;
begin
  RegisterChildNode('matrixrow', TXMLMatrixrowtype);
  inherited;
end;

function TXMLMatrixtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMatrixtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMatrixtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMatrixtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMatrixtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMatrixtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMatrixtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMatrixtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMatrixtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMatrixtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMatrixtype.Get_Matrixrow: IXMLMatrixrowtype;
begin
  Result := ChildNodes['matrixrow'] as IXMLMatrixrowtype;
end;

{ TXMLMatrixrowtype }

procedure TXMLMatrixrowtype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMatrixrowtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMatrixrowtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMatrixrowtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMatrixrowtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMatrixrowtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMatrixrowtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMatrixrowtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMatrixrowtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMatrixrowtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMatrixrowtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMatrixrowtype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLMatrixrowtype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLMatrixrowtype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLMatrixrowtype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLMatrixrowtype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLMatrixrowtype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLMatrixrowtype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLMatrixrowtype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLMatrixrowtype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLMatrixrowtype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLMatrixrowtype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLMatrixrowtype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLMatrixrowtype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLMatrixrowtype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLMatrixrowtype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLMatrixrowtype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLMatrixrowtype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLMatrixrowtype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLMatrixrowtype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLMatrixrowtype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLMatrixrowtype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLMatrixrowtype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLMatrixrowtype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLMatrixrowtype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLMatrixrowtype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLMatrixrowtype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLMatrixrowtype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLMatrixrowtype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLMatrixrowtype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLMatrixrowtype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLMatrixrowtype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLMatrixrowtype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLMatrixrowtype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLMatrixrowtype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLMatrixrowtype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLMatrixrowtype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLMatrixrowtype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLMatrixrowtype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLMatrixrowtype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLMatrixrowtype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLMatrixrowtype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLMatrixrowtype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLMatrixrowtype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLMatrixrowtype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLMatrixrowtype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLMatrixrowtype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLMatrixrowtype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLMatrixrowtype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLMatrixrowtype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLMatrixrowtype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLMatrixrowtype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLMatrixrowtype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLMatrixrowtype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLMatrixrowtype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLMatrixrowtype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLMatrixrowtype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLMatrixrowtype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLMatrixrowtype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLMatrixrowtype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLMatrixrowtype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLMatrixrowtype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLMatrixrowtype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLMatrixrowtype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLMatrixrowtype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLMatrixrowtype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLMatrixrowtype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLMatrixrowtype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLMatrixrowtype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLMatrixrowtype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;

function TXMLMatrixrowtype.Get_Mi: IXMLMitype;
begin
  Result := ChildNodes['mi'] as IXMLMitype;
end;

function TXMLMatrixrowtype.Get_Mo: IXMLMotype;
begin
  Result := ChildNodes['mo'] as IXMLMotype;
end;

function TXMLMatrixrowtype.Get_Mn: IXMLMntype;
begin
  Result := ChildNodes['mn'] as IXMLMntype;
end;

function TXMLMatrixrowtype.Get_Mtext: IXMLMtexttype;
begin
  Result := ChildNodes['mtext'] as IXMLMtexttype;
end;

function TXMLMatrixrowtype.Get_Ms: IXMLMstype;
begin
  Result := ChildNodes['ms'] as IXMLMstype;
end;

function TXMLMatrixrowtype.Get_Mrow: IXMLMrowtype;
begin
  Result := ChildNodes['mrow'] as IXMLMrowtype;
end;

function TXMLMatrixrowtype.Get_Mfrac: IXMLMfractype;
begin
  Result := ChildNodes['mfrac'] as IXMLMfractype;
end;

function TXMLMatrixrowtype.Get_Msqrt: IXMLMsqrttype;
begin
  Result := ChildNodes['msqrt'] as IXMLMsqrttype;
end;

function TXMLMatrixrowtype.Get_Mroot: IXMLMroottype;
begin
  Result := ChildNodes['mroot'] as IXMLMroottype;
end;

function TXMLMatrixrowtype.Get_Mpadded: IXMLMpaddedtype;
begin
  Result := ChildNodes['mpadded'] as IXMLMpaddedtype;
end;

function TXMLMatrixrowtype.Get_Mphantom: IXMLMphantomtype;
begin
  Result := ChildNodes['mphantom'] as IXMLMphantomtype;
end;

function TXMLMatrixrowtype.Get_Mfenced: IXMLMfencedtype;
begin
  Result := ChildNodes['mfenced'] as IXMLMfencedtype;
end;

function TXMLMatrixrowtype.Get_Menclose: IXMLMenclosetype;
begin
  Result := ChildNodes['menclose'] as IXMLMenclosetype;
end;

function TXMLMatrixrowtype.Get_Msub: IXMLMsubtype;
begin
  Result := ChildNodes['msub'] as IXMLMsubtype;
end;

function TXMLMatrixrowtype.Get_Msup: IXMLMsuptype;
begin
  Result := ChildNodes['msup'] as IXMLMsuptype;
end;

function TXMLMatrixrowtype.Get_Msubsup: IXMLMsubsuptype;
begin
  Result := ChildNodes['msubsup'] as IXMLMsubsuptype;
end;

function TXMLMatrixrowtype.Get_Munder: IXMLMundertype;
begin
  Result := ChildNodes['munder'] as IXMLMundertype;
end;

function TXMLMatrixrowtype.Get_Mover: IXMLMovertype;
begin
  Result := ChildNodes['mover'] as IXMLMovertype;
end;

function TXMLMatrixrowtype.Get_Munderover: IXMLMunderovertype;
begin
  Result := ChildNodes['munderover'] as IXMLMunderovertype;
end;

function TXMLMatrixrowtype.Get_Mmultiscripts: IXMLMmultiscriptstype;
begin
  Result := ChildNodes['mmultiscripts'] as IXMLMmultiscriptstype;
end;

function TXMLMatrixrowtype.Get_Mtable: IXMLMtabletype;
begin
  Result := ChildNodes['mtable'] as IXMLMtabletype;
end;

function TXMLMatrixrowtype.Get_Maligngroup: IXMLMaligngrouptype;
begin
  Result := ChildNodes['maligngroup'] as IXMLMaligngrouptype;
end;

function TXMLMatrixrowtype.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMatrixrowtype.Get_Mspace: IXMLMspacetype;
begin
  Result := ChildNodes['mspace'] as IXMLMspacetype;
end;

function TXMLMatrixrowtype.Get_Maction: IXMLMactiontype;
begin
  Result := ChildNodes['maction'] as IXMLMactiontype;
end;

function TXMLMatrixrowtype.Get_Merror: IXMLMerrortype;
begin
  Result := ChildNodes['merror'] as IXMLMerrortype;
end;

function TXMLMatrixrowtype.Get_Mstyle: IXMLMstyletype;
begin
  Result := ChildNodes['mstyle'] as IXMLMstyletype;
end;

{ TXMLDeterminanttype }

function TXMLDeterminanttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLDeterminanttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLDeterminanttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLDeterminanttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLDeterminanttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLDeterminanttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLDeterminanttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDeterminanttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLDeterminanttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLDeterminanttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLDeterminanttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDeterminanttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLDeterminanttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLDeterminanttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

 { TXMLMomentabouttype }

procedure TXMLMomentabouttype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMomentabouttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLMomentabouttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLMomentabouttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLMomentabouttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLMomentabouttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMomentabouttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMomentabouttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMomentabouttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMomentabouttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMomentabouttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMomentabouttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMomentabouttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMomentabouttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMomentabouttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMomentabouttype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLMomentabouttype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLMomentabouttype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLMomentabouttype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLMomentabouttype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLMomentabouttype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLMomentabouttype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLMomentabouttype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLMomentabouttype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLMomentabouttype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLMomentabouttype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLMomentabouttype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLMomentabouttype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLMomentabouttype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLMomentabouttype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLMomentabouttype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLMomentabouttype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLMomentabouttype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLMomentabouttype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLMomentabouttype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLMomentabouttype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLMomentabouttype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLMomentabouttype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLMomentabouttype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLMomentabouttype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLMomentabouttype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLMomentabouttype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLMomentabouttype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLMomentabouttype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLMomentabouttype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLMomentabouttype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLMomentabouttype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLMomentabouttype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLMomentabouttype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLMomentabouttype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLMomentabouttype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLMomentabouttype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLMomentabouttype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLMomentabouttype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLMomentabouttype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLMomentabouttype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLMomentabouttype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLMomentabouttype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLMomentabouttype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLMomentabouttype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLMomentabouttype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLMomentabouttype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLMomentabouttype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLMomentabouttype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLMomentabouttype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLMomentabouttype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLMomentabouttype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLMomentabouttype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLMomentabouttype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLMomentabouttype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLMomentabouttype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLMomentabouttype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLMomentabouttype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLMomentabouttype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLMomentabouttype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLMomentabouttype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLMomentabouttype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLMomentabouttype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLMomentabouttype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLMomentabouttype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLMomentabouttype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLMomentabouttype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLMomentabouttype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLMomentabouttype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLApplytype }

procedure TXMLApplytype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLApplytype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLApplytype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLApplytype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLApplytype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLApplytype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLApplytype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLApplytype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLApplytype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLApplytype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLApplytype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLApplytype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLApplytype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLApplytype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLApplytype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLApplytype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLApplytype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLApplytype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLApplytype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLApplytype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLApplytype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLApplytype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLApplytype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLApplytype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLApplytype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLApplytype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLApplytype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLApplytype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLApplytype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLApplytype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLApplytype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLApplytype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLApplytype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLApplytype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLApplytype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLApplytype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLApplytype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLApplytype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLApplytype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLApplytype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLApplytype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLApplytype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLApplytype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLApplytype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLApplytype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLApplytype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLApplytype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLApplytype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLApplytype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLApplytype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLApplytype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLApplytype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLApplytype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLApplytype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLApplytype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLApplytype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLApplytype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLApplytype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLApplytype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLApplytype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLApplytype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLApplytype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLApplytype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLApplytype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLApplytype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLApplytype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLApplytype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLApplytype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLApplytype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLApplytype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLApplytype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLApplytype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLApplytype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLApplytype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLApplytype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLApplytype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLApplytype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLApplytype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLApplytype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLApplytype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLIntervaltype }

procedure TXMLIntervaltype.AfterConstruction;
begin
  inherited;
end;

function TXMLIntervaltype.Get_Closure: UnicodeString;
begin
  Result := AttributeNodes['closure'].Text;
end;

procedure TXMLIntervaltype.Set_Closure(Value: UnicodeString);
begin
  SetAttribute('closure', Value);
end;

function TXMLIntervaltype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLIntervaltype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLIntervaltype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLIntervaltype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLIntervaltype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLIntervaltype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLIntervaltype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLIntervaltype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLIntervaltype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLIntervaltype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLPiecetype }

procedure TXMLPiecetype.AfterConstruction;
begin
  inherited;
end;


{ TXMLPiecetypeList }

function TXMLPiecetypeList.Add: IXMLPiecetype;
begin
  Result := AddItem(-1) as IXMLPiecetype;
end;

function TXMLPiecetypeList.Insert(const Index: Integer): IXMLPiecetype;
begin
  Result := AddItem(Index) as IXMLPiecetype;
end;

function TXMLPiecetypeList.Get_Item(Index: Integer): IXMLPiecetype;
begin
  Result := List[Index] as IXMLPiecetype;
end;

{ TXMLMrowtype }

procedure TXMLMrowtype.AfterConstruction;
begin
  inherited;
end;

function TXMLMrowtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMrowtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMrowtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMrowtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMrowtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMrowtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMrowtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMrowtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMrowtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMrowtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMroottype }

procedure TXMLMroottype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMroottype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMroottype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMroottype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMroottype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMroottype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMroottype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMroottype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMroottype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMroottype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMroottype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMpaddedtype }

procedure TXMLMpaddedtype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMpaddedtype.Get_Width: UnicodeString;
begin
  Result := AttributeNodes['width'].Text;
end;

procedure TXMLMpaddedtype.Set_Width(Value: UnicodeString);
begin
  SetAttribute('width', Value);
end;

function TXMLMpaddedtype.Get_Lspace: UnicodeString;
begin
  Result := AttributeNodes['lspace'].Text;
end;

procedure TXMLMpaddedtype.Set_Lspace(Value: UnicodeString);
begin
  SetAttribute('lspace', Value);
end;

function TXMLMpaddedtype.Get_Height: UnicodeString;
begin
  Result := AttributeNodes['height'].Text;
end;

procedure TXMLMpaddedtype.Set_Height(Value: UnicodeString);
begin
  SetAttribute('height', Value);
end;

function TXMLMpaddedtype.Get_Depth: UnicodeString;
begin
  Result := AttributeNodes['depth'].Text;
end;

procedure TXMLMpaddedtype.Set_Depth(Value: UnicodeString);
begin
  SetAttribute('depth', Value);
end;

function TXMLMpaddedtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMpaddedtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMpaddedtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMpaddedtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMpaddedtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMpaddedtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMpaddedtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMpaddedtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMpaddedtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMpaddedtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMphantomtype }

procedure TXMLMphantomtype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMphantomtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMphantomtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMphantomtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMphantomtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMphantomtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMphantomtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMphantomtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMphantomtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMphantomtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMphantomtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMfencedtype }

procedure TXMLMfencedtype.AfterConstruction;
begin
  inherited;
end;

function TXMLMfencedtype.Get_Open: UnicodeString;
begin
  Result := AttributeNodes['open'].Text;
end;

procedure TXMLMfencedtype.Set_Open(Value: UnicodeString);
begin
  SetAttribute('open', Value);
end;

function TXMLMfencedtype.Get_Close: UnicodeString;
begin
  Result := AttributeNodes['close'].Text;
end;

procedure TXMLMfencedtype.Set_Close(Value: UnicodeString);
begin
  SetAttribute('close', Value);
end;

function TXMLMfencedtype.Get_Separators: UnicodeString;
begin
  Result := AttributeNodes['separators'].Text;
end;

procedure TXMLMfencedtype.Set_Separators(Value: UnicodeString);
begin
  SetAttribute('separators', Value);
end;

function TXMLMfencedtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMfencedtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMfencedtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMfencedtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMfencedtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMfencedtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMfencedtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMfencedtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMfencedtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMfencedtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMenclosetype }

procedure TXMLMenclosetype.AfterConstruction;
begin
  inherited;
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMenclosetype.Get_Notation: UnicodeString;
begin
  Result := AttributeNodes['notation'].Text;
end;

procedure TXMLMenclosetype.Set_Notation(Value: UnicodeString);
begin
  SetAttribute('notation', Value);
end;

function TXMLMenclosetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMenclosetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMenclosetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMenclosetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMenclosetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMenclosetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMenclosetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMenclosetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMenclosetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMenclosetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;
{ TXMLBvartype }

procedure TXMLBvartype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLBvartype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLBvartype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLBvartype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLBvartype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLBvartype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLBvartype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLBvartype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLBvartype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLBvartype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLBvartype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLBvartype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLBvartype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLBvartype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLBvartype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLBvartype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLBvartype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLBvartype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLBvartype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLBvartype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLBvartype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLBvartype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLBvartype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLBvartype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLBvartype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLBvartype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLBvartype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLBvartype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLBvartype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLBvartype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLBvartype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLBvartype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLBvartype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLBvartype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLBvartype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLBvartype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLBvartype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLBvartype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLBvartype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLBvartype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLBvartype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLBvartype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLBvartype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLBvartype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLBvartype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLBvartype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLBvartype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLBvartype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLBvartype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLBvartype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLBvartype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLBvartype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLBvartype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLBvartype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLBvartype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLBvartype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLBvartype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLBvartype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLBvartype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLBvartype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLBvartype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLBvartype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLBvartype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLBvartype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLBvartype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLBvartype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLBvartype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLBvartype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLBvartype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLBvartype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLBvartype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLBvartype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLBvartype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLBvartype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLBvartype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLBvartype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLBvartype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLBvartype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLBvartype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLBvartype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;

{ TXMLDegreetype }

procedure TXMLDegreetype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLDegreetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLDegreetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLDegreetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDegreetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLDegreetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLDegreetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLDegreetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDegreetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLDegreetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLDegreetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLDegreetype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLDegreetype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLDegreetype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLDegreetype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLDegreetype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLDegreetype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLDegreetype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLDegreetype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLDegreetype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLDegreetype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLDegreetype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLDegreetype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLDegreetype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLDegreetype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLDegreetype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLDegreetype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLDegreetype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLDegreetype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLDegreetype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLDegreetype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLDegreetype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLDegreetype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLDegreetype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLDegreetype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLDegreetype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLDegreetype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLDegreetype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLDegreetype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLDegreetype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLDegreetype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLDegreetype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLDegreetype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLDegreetype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLDegreetype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLDegreetype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLDegreetype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLDegreetype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLDegreetype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLDegreetype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLDegreetype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLDegreetype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLDegreetype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLDegreetype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLDegreetype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLDegreetype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLDegreetype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLDegreetype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLDegreetype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLDegreetype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLDegreetype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLDegreetype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLDegreetype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLDegreetype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLDegreetype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLDegreetype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLDegreetype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLDegreetype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLDegreetype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLDegreetype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLDegreetype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLDegreetype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLDegreetype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLDegreetype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLDegreetype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLDegreetype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLDegreetype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLDegreetype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLDegreetype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLDegreetype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


{ TXMLSemanticstype }

procedure TXMLSemanticstype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  RegisterChildNode('annotation', TXMLAnnotationtype);
  RegisterChildNode('annotation-xml', TXMLAnnotationxmltype);
  inherited;
end;

function TXMLSemanticstype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLSemanticstype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLSemanticstype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLSemanticstype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLSemanticstype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLSemanticstype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLSemanticstype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSemanticstype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLSemanticstype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLSemanticstype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLSemanticstype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSemanticstype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLSemanticstype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLSemanticstype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLSemanticstype.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLSemanticstype.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLSemanticstype.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLSemanticstype.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLSemanticstype.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLSemanticstype.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLSemanticstype.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLSemanticstype.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLSemanticstype.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLSemanticstype.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLSemanticstype.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLSemanticstype.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLSemanticstype.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLSemanticstype.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLSemanticstype.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLSemanticstype.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLSemanticstype.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;

function TXMLSemanticstype.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLSemanticstype.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLSemanticstype.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLSemanticstype.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLSemanticstype.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLSemanticstype.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLSemanticstype.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLSemanticstype.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLSemanticstype.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLSemanticstype.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLSemanticstype.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLSemanticstype.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLSemanticstype.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;

function TXMLSemanticstype.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLSemanticstype.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLSemanticstype.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLSemanticstype.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLSemanticstype.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLSemanticstype.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLSemanticstype.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLSemanticstype.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLSemanticstype.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLSemanticstype.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLSemanticstype.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLSemanticstype.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLSemanticstype.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLSemanticstype.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLSemanticstype.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLSemanticstype.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLSemanticstype.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLSemanticstype.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLSemanticstype.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLSemanticstype.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLSemanticstype.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLSemanticstype.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLSemanticstype.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLSemanticstype.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLSemanticstype.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLSemanticstype.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLSemanticstype.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLSemanticstype.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLSemanticstype.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLSemanticstype.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLSemanticstype.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLSemanticstype.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLSemanticstype.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLSemanticstype.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLSemanticstype.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLSemanticstype.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLSemanticstype.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLSemanticstype.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLSemanticstype.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


function TXMLSemanticstype.Get_Annotation: IXMLAnnotationtype;
begin
  Result := ChildNodes['annotation'] as IXMLAnnotationtype;
end;

function TXMLSemanticstype.Get_Annotationxml: IXMLAnnotationxmltype;
begin
  Result := ChildNodes['annotation-xml'] as IXMLAnnotationxmltype;
end;

{ TXMLOtherwisetype }

procedure TXMLOtherwisetype.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLOtherwisetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLOtherwisetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLOtherwisetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLOtherwisetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLOtherwisetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLOtherwisetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLOtherwisetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLOtherwisetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLOtherwisetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLOtherwisetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLMitype }

procedure TXMLMitype.AfterConstruction;
begin
  RegisterChildNode('malignmark', TXMLMalignmarktype);
  RegisterChildNode('mglyph', TXMLMglyphtype);
  inherited;
end;

function TXMLMitype.Get_Mathvariant: UnicodeString;
begin
  Result := AttributeNodes['mathvariant'].Text;
end;

procedure TXMLMitype.Set_Mathvariant(Value: UnicodeString);
begin
  SetAttribute('mathvariant', Value);
end;

function TXMLMitype.Get_Mathsize: UnicodeString;
begin
  Result := AttributeNodes['mathsize'].Text;
end;

procedure TXMLMitype.Set_Mathsize(Value: UnicodeString);
begin
  SetAttribute('mathsize', Value);
end;

function TXMLMitype.Get_Mathcolor: UnicodeString;
begin
  Result := AttributeNodes['mathcolor'].Text;
end;

procedure TXMLMitype.Set_Mathcolor(Value: UnicodeString);
begin
  SetAttribute('mathcolor', Value);
end;

function TXMLMitype.Get_Mathbackground: UnicodeString;
begin
  Result := AttributeNodes['mathbackground'].Text;
end;

procedure TXMLMitype.Set_Mathbackground(Value: UnicodeString);
begin
  SetAttribute('mathbackground', Value);
end;

function TXMLMitype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMitype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMitype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMitype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMitype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMitype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMitype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMitype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMitype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMitype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMitype.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMitype.Get_Mglyph: IXMLMglyphtype;
begin
  Result := ChildNodes['mglyph'] as IXMLMglyphtype;
end;

{ TXMLMalignmarktype }

function TXMLMalignmarktype.Get_Edge: UnicodeString;
begin
  Result := AttributeNodes['edge'].Text;
end;

procedure TXMLMalignmarktype.Set_Edge(Value: UnicodeString);
begin
  SetAttribute('edge', Value);
end;

function TXMLMalignmarktype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMalignmarktype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMalignmarktype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMalignmarktype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMalignmarktype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMalignmarktype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMalignmarktype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMalignmarktype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMalignmarktype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMalignmarktype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMotype }

procedure TXMLMotype.AfterConstruction;
begin
  RegisterChildNode('malignmark', TXMLMalignmarktype);
  RegisterChildNode('mglyph', TXMLMglyphtype);
  inherited;
end;

function TXMLMotype.Get_Form: UnicodeString;
begin
  Result := AttributeNodes['form'].Text;
end;

procedure TXMLMotype.Set_Form(Value: UnicodeString);
begin
  SetAttribute('form', Value);
end;

function TXMLMotype.Get_Lspace: UnicodeString;
begin
  Result := AttributeNodes['lspace'].Text;
end;

procedure TXMLMotype.Set_Lspace(Value: UnicodeString);
begin
  SetAttribute('lspace', Value);
end;

function TXMLMotype.Get_Rspace: UnicodeString;
begin
  Result := AttributeNodes['rspace'].Text;
end;

procedure TXMLMotype.Set_Rspace(Value: UnicodeString);
begin
  SetAttribute('rspace', Value);
end;

function TXMLMotype.Get_Fence: Boolean;
begin
  Result := AttributeNodes['fence'].NodeValue;
end;

procedure TXMLMotype.Set_Fence(Value: Boolean);
begin
  SetAttribute('fence', Value);
end;

function TXMLMotype.Get_Separator: Boolean;
begin
  Result := AttributeNodes['separator'].NodeValue;
end;

procedure TXMLMotype.Set_Separator(Value: Boolean);
begin
  SetAttribute('separator', Value);
end;

function TXMLMotype.Get_Stretchy: Boolean;
begin
  Result := AttributeNodes['stretchy'].NodeValue;
end;

procedure TXMLMotype.Set_Stretchy(Value: Boolean);
begin
  SetAttribute('stretchy', Value);
end;

function TXMLMotype.Get_Symmetric: Boolean;
begin
  Result := AttributeNodes['symmetric'].NodeValue;
end;

procedure TXMLMotype.Set_Symmetric(Value: Boolean);
begin
  SetAttribute('symmetric', Value);
end;

function TXMLMotype.Get_Movablelimits: Boolean;
begin
  Result := AttributeNodes['movablelimits'].NodeValue;
end;

procedure TXMLMotype.Set_Movablelimits(Value: Boolean);
begin
  SetAttribute('movablelimits', Value);
end;

function TXMLMotype.Get_Accent: Boolean;
begin
  Result := AttributeNodes['accent'].NodeValue;
end;

procedure TXMLMotype.Set_Accent(Value: Boolean);
begin
  SetAttribute('accent', Value);
end;

function TXMLMotype.Get_Largeop: Boolean;
begin
  Result := AttributeNodes['largeop'].NodeValue;
end;

procedure TXMLMotype.Set_Largeop(Value: Boolean);
begin
  SetAttribute('largeop', Value);
end;

function TXMLMotype.Get_Minsize: UnicodeString;
begin
  Result := AttributeNodes['minsize'].Text;
end;

procedure TXMLMotype.Set_Minsize(Value: UnicodeString);
begin
  SetAttribute('minsize', Value);
end;

function TXMLMotype.Get_Maxsize: UnicodeString;
begin
  Result := AttributeNodes['maxsize'].Text;
end;

procedure TXMLMotype.Set_Maxsize(Value: UnicodeString);
begin
  SetAttribute('maxsize', Value);
end;

function TXMLMotype.Get_Mathvariant: UnicodeString;
begin
  Result := AttributeNodes['mathvariant'].Text;
end;

procedure TXMLMotype.Set_Mathvariant(Value: UnicodeString);
begin
  SetAttribute('mathvariant', Value);
end;

function TXMLMotype.Get_Mathsize: UnicodeString;
begin
  Result := AttributeNodes['mathsize'].Text;
end;

procedure TXMLMotype.Set_Mathsize(Value: UnicodeString);
begin
  SetAttribute('mathsize', Value);
end;

function TXMLMotype.Get_Mathcolor: UnicodeString;
begin
  Result := AttributeNodes['mathcolor'].Text;
end;

procedure TXMLMotype.Set_Mathcolor(Value: UnicodeString);
begin
  SetAttribute('mathcolor', Value);
end;

function TXMLMotype.Get_Mathbackground: UnicodeString;
begin
  Result := AttributeNodes['mathbackground'].Text;
end;

procedure TXMLMotype.Set_Mathbackground(Value: UnicodeString);
begin
  SetAttribute('mathbackground', Value);
end;

function TXMLMotype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMotype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMotype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMotype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMotype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMotype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMotype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMotype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMotype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMotype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMotype.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMotype.Get_Mglyph: IXMLMglyphtype;
begin
  Result := ChildNodes['mglyph'] as IXMLMglyphtype;
end;

{ TXMLMntype }

procedure TXMLMntype.AfterConstruction;
begin
  RegisterChildNode('malignmark', TXMLMalignmarktype);
  RegisterChildNode('mglyph', TXMLMglyphtype);
  inherited;
end;

function TXMLMntype.Get_Mathvariant: UnicodeString;
begin
  Result := AttributeNodes['mathvariant'].Text;
end;

procedure TXMLMntype.Set_Mathvariant(Value: UnicodeString);
begin
  SetAttribute('mathvariant', Value);
end;

function TXMLMntype.Get_Mathsize: UnicodeString;
begin
  Result := AttributeNodes['mathsize'].Text;
end;

procedure TXMLMntype.Set_Mathsize(Value: UnicodeString);
begin
  SetAttribute('mathsize', Value);
end;

function TXMLMntype.Get_Mathcolor: UnicodeString;
begin
  Result := AttributeNodes['mathcolor'].Text;
end;

procedure TXMLMntype.Set_Mathcolor(Value: UnicodeString);
begin
  SetAttribute('mathcolor', Value);
end;

function TXMLMntype.Get_Mathbackground: UnicodeString;
begin
  Result := AttributeNodes['mathbackground'].Text;
end;

procedure TXMLMntype.Set_Mathbackground(Value: UnicodeString);
begin
  SetAttribute('mathbackground', Value);
end;

function TXMLMntype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMntype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMntype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMntype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMntype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMntype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMntype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMntype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMntype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMntype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMntype.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMntype.Get_Mglyph: IXMLMglyphtype;
begin
  Result := ChildNodes['mglyph'] as IXMLMglyphtype;
end;

{ TXMLMtexttype }

procedure TXMLMtexttype.AfterConstruction;
begin
  RegisterChildNode('malignmark', TXMLMalignmarktype);
  RegisterChildNode('mglyph', TXMLMglyphtype);
  inherited;
end;

function TXMLMtexttype.Get_Mathvariant: UnicodeString;
begin
  Result := AttributeNodes['mathvariant'].Text;
end;

procedure TXMLMtexttype.Set_Mathvariant(Value: UnicodeString);
begin
  SetAttribute('mathvariant', Value);
end;

function TXMLMtexttype.Get_Mathsize: UnicodeString;
begin
  Result := AttributeNodes['mathsize'].Text;
end;

procedure TXMLMtexttype.Set_Mathsize(Value: UnicodeString);
begin
  SetAttribute('mathsize', Value);
end;

function TXMLMtexttype.Get_Mathcolor: UnicodeString;
begin
  Result := AttributeNodes['mathcolor'].Text;
end;

procedure TXMLMtexttype.Set_Mathcolor(Value: UnicodeString);
begin
  SetAttribute('mathcolor', Value);
end;

function TXMLMtexttype.Get_Mathbackground: UnicodeString;
begin
  Result := AttributeNodes['mathbackground'].Text;
end;

procedure TXMLMtexttype.Set_Mathbackground(Value: UnicodeString);
begin
  SetAttribute('mathbackground', Value);
end;

function TXMLMtexttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMtexttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMtexttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMtexttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMtexttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMtexttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMtexttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMtexttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMtexttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMtexttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMtexttype.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMtexttype.Get_Mglyph: IXMLMglyphtype;
begin
  Result := ChildNodes['mglyph'] as IXMLMglyphtype;
end;

{ TXMLMstype }

procedure TXMLMstype.AfterConstruction;
begin
  RegisterChildNode('malignmark', TXMLMalignmarktype);
  RegisterChildNode('mglyph', TXMLMglyphtype);
  inherited;
end;

function TXMLMstype.Get_Lquote: UnicodeString;
begin
  Result := AttributeNodes['lquote'].Text;
end;

procedure TXMLMstype.Set_Lquote(Value: UnicodeString);
begin
  SetAttribute('lquote', Value);
end;

function TXMLMstype.Get_Rquote: UnicodeString;
begin
  Result := AttributeNodes['rquote'].Text;
end;

procedure TXMLMstype.Set_Rquote(Value: UnicodeString);
begin
  SetAttribute('rquote', Value);
end;

function TXMLMstype.Get_Mathvariant: UnicodeString;
begin
  Result := AttributeNodes['mathvariant'].Text;
end;

procedure TXMLMstype.Set_Mathvariant(Value: UnicodeString);
begin
  SetAttribute('mathvariant', Value);
end;

function TXMLMstype.Get_Mathsize: UnicodeString;
begin
  Result := AttributeNodes['mathsize'].Text;
end;

procedure TXMLMstype.Set_Mathsize(Value: UnicodeString);
begin
  SetAttribute('mathsize', Value);
end;

function TXMLMstype.Get_Mathcolor: UnicodeString;
begin
  Result := AttributeNodes['mathcolor'].Text;
end;

procedure TXMLMstype.Set_Mathcolor(Value: UnicodeString);
begin
  SetAttribute('mathcolor', Value);
end;

function TXMLMstype.Get_Mathbackground: UnicodeString;
begin
  Result := AttributeNodes['mathbackground'].Text;
end;

procedure TXMLMstype.Set_Mathbackground(Value: UnicodeString);
begin
  SetAttribute('mathbackground', Value);
end;

function TXMLMstype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMstype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMstype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMstype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMstype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMstype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMstype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMstype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMstype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMstype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMstype.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMstype.Get_Mglyph: IXMLMglyphtype;
begin
  Result := ChildNodes['mglyph'] as IXMLMglyphtype;
end;


{ TXMLMtabletype }

procedure TXMLMtabletype.AfterConstruction;
begin
  RegisterChildNode('mtr', TXMLMtrtype);
  RegisterChildNode('mlabeledtr', TXMLMlabeledtrtype);
  inherited;
end;

function TXMLMtabletype.Get_Rowalign: UnicodeString;
begin
  Result := AttributeNodes['rowalign'].Text;
end;

procedure TXMLMtabletype.Set_Rowalign(Value: UnicodeString);
begin
  SetAttribute('rowalign', Value);
end;

function TXMLMtabletype.Get_Columnalign: UnicodeString;
begin
  Result := AttributeNodes['columnalign'].Text;
end;

procedure TXMLMtabletype.Set_Columnalign(Value: UnicodeString);
begin
  SetAttribute('columnalign', Value);
end;

function TXMLMtabletype.Get_Groupalign: UnicodeString;
begin
  Result := AttributeNodes['groupalign'].Text;
end;

procedure TXMLMtabletype.Set_Groupalign(Value: UnicodeString);
begin
  SetAttribute('groupalign', Value);
end;

function TXMLMtabletype.Get_Align: UnicodeString;
begin
  Result := AttributeNodes['align'].Text;
end;

procedure TXMLMtabletype.Set_Align(Value: UnicodeString);
begin
  SetAttribute('align', Value);
end;

function TXMLMtabletype.Get_Alignmentscope: UnicodeString;
begin
  Result := AttributeNodes['alignmentscope'].Text;
end;

procedure TXMLMtabletype.Set_Alignmentscope(Value: UnicodeString);
begin
  SetAttribute('alignmentscope', Value);
end;

function TXMLMtabletype.Get_Columnwidth: UnicodeString;
begin
  Result := AttributeNodes['columnwidth'].Text;
end;

procedure TXMLMtabletype.Set_Columnwidth(Value: UnicodeString);
begin
  SetAttribute('columnwidth', Value);
end;

function TXMLMtabletype.Get_Width: UnicodeString;
begin
  Result := AttributeNodes['width'].Text;
end;

procedure TXMLMtabletype.Set_Width(Value: UnicodeString);
begin
  SetAttribute('width', Value);
end;

function TXMLMtabletype.Get_Rowspacing: UnicodeString;
begin
  Result := AttributeNodes['rowspacing'].Text;
end;

procedure TXMLMtabletype.Set_Rowspacing(Value: UnicodeString);
begin
  SetAttribute('rowspacing', Value);
end;

function TXMLMtabletype.Get_Columnspacing: UnicodeString;
begin
  Result := AttributeNodes['columnspacing'].Text;
end;

procedure TXMLMtabletype.Set_Columnspacing(Value: UnicodeString);
begin
  SetAttribute('columnspacing', Value);
end;

function TXMLMtabletype.Get_Rowlines: UnicodeString;
begin
  Result := AttributeNodes['rowlines'].Text;
end;

procedure TXMLMtabletype.Set_Rowlines(Value: UnicodeString);
begin
  SetAttribute('rowlines', Value);
end;

function TXMLMtabletype.Get_Columnlines: UnicodeString;
begin
  Result := AttributeNodes['columnlines'].Text;
end;

procedure TXMLMtabletype.Set_Columnlines(Value: UnicodeString);
begin
  SetAttribute('columnlines', Value);
end;

function TXMLMtabletype.Get_Frame: UnicodeString;
begin
  Result := AttributeNodes['frame'].Text;
end;

procedure TXMLMtabletype.Set_Frame(Value: UnicodeString);
begin
  SetAttribute('frame', Value);
end;

function TXMLMtabletype.Get_Framespacing: UnicodeString;
begin
  Result := AttributeNodes['framespacing'].Text;
end;

procedure TXMLMtabletype.Set_Framespacing(Value: UnicodeString);
begin
  SetAttribute('framespacing', Value);
end;

function TXMLMtabletype.Get_Equalrows: Boolean;
begin
  Result := AttributeNodes['equalrows'].NodeValue;
end;

procedure TXMLMtabletype.Set_Equalrows(Value: Boolean);
begin
  SetAttribute('equalrows', Value);
end;

function TXMLMtabletype.Get_Equalcolumns: Boolean;
begin
  Result := AttributeNodes['equalcolumns'].NodeValue;
end;

procedure TXMLMtabletype.Set_Equalcolumns(Value: Boolean);
begin
  SetAttribute('equalcolumns', Value);
end;

function TXMLMtabletype.Get_Displaystyle: Boolean;
begin
  Result := AttributeNodes['displaystyle'].NodeValue;
end;

procedure TXMLMtabletype.Set_Displaystyle(Value: Boolean);
begin
  SetAttribute('displaystyle', Value);
end;

function TXMLMtabletype.Get_Side: UnicodeString;
begin
  Result := AttributeNodes['side'].Text;
end;

procedure TXMLMtabletype.Set_Side(Value: UnicodeString);
begin
  SetAttribute('side', Value);
end;

function TXMLMtabletype.Get_Minlabelspacing: UnicodeString;
begin
  Result := AttributeNodes['minlabelspacing'].Text;
end;

procedure TXMLMtabletype.Set_Minlabelspacing(Value: UnicodeString);
begin
  SetAttribute('minlabelspacing', Value);
end;

function TXMLMtabletype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMtabletype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMtabletype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMtabletype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMtabletype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMtabletype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMtabletype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMtabletype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMtabletype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMtabletype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMtabletype.Get_Mtr: IXMLMtrtype;
begin
  Result := ChildNodes['mtr'] as IXMLMtrtype;
end;

function TXMLMtabletype.Get_Mlabeledtr: IXMLMlabeledtrtype;
begin
  Result := ChildNodes['mlabeledtr'] as IXMLMlabeledtrtype;
end;


{ TXMLTransposetype }

function TXMLTransposetype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLTransposetype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLTransposetype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLTransposetype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLTransposetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLTransposetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLTransposetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLTransposetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLTransposetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLTransposetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLTransposetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLTransposetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLTransposetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLTransposetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLSelectortype }

function TXMLSelectortype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLSelectortype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLSelectortype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLSelectortype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLSelectortype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLSelectortype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLSelectortype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSelectortype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLSelectortype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLSelectortype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLSelectortype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSelectortype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLSelectortype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLSelectortype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLVectorproducttype }

function TXMLVectorproducttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLVectorproducttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLVectorproducttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLVectorproducttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLVectorproducttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLVectorproducttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLVectorproducttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLVectorproducttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLVectorproducttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLVectorproducttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLVectorproducttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLVectorproducttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLVectorproducttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLVectorproducttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLScalarproducttype }

function TXMLScalarproducttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLScalarproducttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLScalarproducttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLScalarproducttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLScalarproducttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLScalarproducttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLScalarproducttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLScalarproducttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLScalarproducttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLScalarproducttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLScalarproducttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLScalarproducttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLScalarproducttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLScalarproducttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLOuterproducttype }

function TXMLOuterproducttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLOuterproducttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLOuterproducttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLOuterproducttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLOuterproducttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLOuterproducttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLOuterproducttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLOuterproducttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLOuterproducttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLOuterproducttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLOuterproducttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLOuterproducttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLOuterproducttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLOuterproducttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLDivergencetype }

function TXMLDivergencetype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLDivergencetype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLDivergencetype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLDivergencetype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLDivergencetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLDivergencetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLDivergencetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDivergencetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLDivergencetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLDivergencetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLDivergencetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLDivergencetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLDivergencetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLDivergencetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLGradtype }

function TXMLGradtype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLGradtype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLGradtype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLGradtype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLGradtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLGradtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLGradtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLGradtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLGradtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLGradtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLGradtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLGradtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLGradtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLGradtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLCurltype }

function TXMLCurltype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLCurltype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLCurltype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLCurltype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLCurltype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLCurltype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLCurltype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLCurltype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLCurltype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLCurltype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLCurltype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLCurltype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLCurltype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLCurltype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLLaplaciantype }

function TXMLLaplaciantype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLLaplaciantype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLLaplaciantype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLLaplaciantype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLLaplaciantype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLLaplaciantype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLLaplaciantype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLLaplaciantype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLLaplaciantype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLLaplaciantype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLLaplaciantype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLLaplaciantype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLLaplaciantype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLLaplaciantype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMeantype }

function TXMLMeantype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLMeantype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLMeantype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLMeantype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLMeantype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMeantype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMeantype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMeantype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMeantype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMeantype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMeantype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMeantype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMeantype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMeantype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLSdevtype }

function TXMLSdevtype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLSdevtype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLSdevtype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLSdevtype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLSdevtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLSdevtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLSdevtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSdevtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLSdevtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLSdevtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLSdevtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLSdevtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLSdevtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLSdevtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLVariancetype }

function TXMLVariancetype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLVariancetype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLVariancetype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLVariancetype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLVariancetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLVariancetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLVariancetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLVariancetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLVariancetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLVariancetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLVariancetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLVariancetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLVariancetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLVariancetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMediantype }

function TXMLMediantype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLMediantype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLMediantype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLMediantype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLMediantype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMediantype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMediantype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMediantype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMediantype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMediantype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMediantype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMediantype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMediantype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMediantype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLModetype }

function TXMLModetype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLModetype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLModetype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLModetype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLModetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLModetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLModetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLModetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLModetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLModetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLModetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLModetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLModetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLModetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLMomenttype }

function TXMLMomenttype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLMomenttype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLMomenttype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLMomenttype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLMomenttype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMomenttype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMomenttype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMomenttype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMomenttype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMomenttype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMomenttype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMomenttype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMomenttype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMomenttype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLInversetype }

function TXMLInversetype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLInversetype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLInversetype.Get_DefinitionURL: UnicodeString;
begin
  Result := AttributeNodes['definitionURL'].Text;
end;

procedure TXMLInversetype.Set_DefinitionURL(Value: UnicodeString);
begin
  SetAttribute('definitionURL', Value);
end;

function TXMLInversetype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLInversetype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLInversetype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLInversetype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLInversetype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLInversetype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLInversetype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLInversetype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLInversetype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLInversetype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;


{ TXMLAnnotationtype }

function TXMLAnnotationtype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLAnnotationtype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLAnnotationtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLAnnotationtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLAnnotationtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLAnnotationtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLAnnotationtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLAnnotationtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLAnnotationtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLAnnotationtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLAnnotationtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLAnnotationtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

{ TXMLAnnotationxmltype }

function TXMLAnnotationxmltype.Get_Encoding: UnicodeString;
begin
  Result := AttributeNodes['encoding'].Text;
end;

procedure TXMLAnnotationxmltype.Set_Encoding(Value: UnicodeString);
begin
  SetAttribute('encoding', Value);
end;

function TXMLAnnotationxmltype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLAnnotationxmltype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLAnnotationxmltype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLAnnotationxmltype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLAnnotationxmltype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLAnnotationxmltype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLAnnotationxmltype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLAnnotationxmltype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLAnnotationxmltype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLAnnotationxmltype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;
{ TXMLMglyphtype }

function TXMLMglyphtype.Get_Alt: UnicodeString;
begin
  Result := AttributeNodes['alt'].Text;
end;

procedure TXMLMglyphtype.Set_Alt(Value: UnicodeString);
begin
  SetAttribute('alt', Value);
end;

function TXMLMglyphtype.Get_Fontfamily: UnicodeString;
begin
  Result := AttributeNodes['fontfamily'].Text;
end;

procedure TXMLMglyphtype.Set_Fontfamily(Value: UnicodeString);
begin
  SetAttribute('fontfamily', Value);
end;

function TXMLMglyphtype.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLMglyphtype.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;



{ TXMLMtrtype }

procedure TXMLMtrtype.AfterConstruction;
begin
  RegisterChildNode('mtd', TXMLMtdtype);
  inherited;
end;

function TXMLMtrtype.Get_Rowalign: UnicodeString;
begin
  Result := AttributeNodes['rowalign'].Text;
end;

procedure TXMLMtrtype.Set_Rowalign(Value: UnicodeString);
begin
  SetAttribute('rowalign', Value);
end;

function TXMLMtrtype.Get_Columnalign: UnicodeString;
begin
  Result := AttributeNodes['columnalign'].Text;
end;

procedure TXMLMtrtype.Set_Columnalign(Value: UnicodeString);
begin
  SetAttribute('columnalign', Value);
end;

function TXMLMtrtype.Get_Groupalign: UnicodeString;
begin
  Result := AttributeNodes['groupalign'].Text;
end;

procedure TXMLMtrtype.Set_Groupalign(Value: UnicodeString);
begin
  SetAttribute('groupalign', Value);
end;

function TXMLMtrtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMtrtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMtrtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMtrtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMtrtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMtrtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMtrtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMtrtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMtrtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMtrtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMtrtype.Get_Mtd: IXMLMtdtype;
begin
  Result := ChildNodes['mtd'] as IXMLMtdtype;
end;

{ TXMLMlabeledtrtype }

procedure TXMLMlabeledtrtype.AfterConstruction;
begin
  RegisterChildNode('mtd', TXMLMtdtype);
  inherited;
end;

function TXMLMlabeledtrtype.Get_Rowalign: UnicodeString;
begin
  Result := AttributeNodes['rowalign'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Rowalign(Value: UnicodeString);
begin
  SetAttribute('rowalign', Value);
end;

function TXMLMlabeledtrtype.Get_Columnalign: UnicodeString;
begin
  Result := AttributeNodes['columnalign'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Columnalign(Value: UnicodeString);
begin
  SetAttribute('columnalign', Value);
end;

function TXMLMlabeledtrtype.Get_Groupalign: UnicodeString;
begin
  Result := AttributeNodes['groupalign'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Groupalign(Value: UnicodeString);
begin
  SetAttribute('groupalign', Value);
end;

function TXMLMlabeledtrtype.Get_Class_: UnicodeString;
begin
  Result := AttributeNodes['class'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Class_(Value: UnicodeString);
begin
  SetAttribute('class', Value);
end;

function TXMLMlabeledtrtype.Get_Style: UnicodeString;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Style(Value: UnicodeString);
begin
  SetAttribute('style', Value);
end;

function TXMLMlabeledtrtype.Get_Xref: UnicodeString;
begin
  Result := AttributeNodes['xref'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Xref(Value: UnicodeString);
begin
  SetAttribute('xref', Value);
end;

function TXMLMlabeledtrtype.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLMlabeledtrtype.Get_Href: UnicodeString;
begin
  Result := AttributeNodes['href'].Text;
end;

procedure TXMLMlabeledtrtype.Set_Href(Value: UnicodeString);
begin
  SetAttribute('href', Value);
end;

function TXMLMlabeledtrtype.Get_Mtd: IXMLMtdtype;
begin
  Result := ChildNodes['mtd'] as IXMLMtdtype;
end;


{ TXMLMathConsttype }

procedure TXMLMathBaseType.AfterConstruction;
begin
  RegisterChildNode('cn', TXMLCntype);
  RegisterChildNode('ci', TXMLCitype);
  RegisterChildNode('csymbol', TXMLCsymboltype);
  RegisterChildNode('abs', TXMLArithtype);
  RegisterChildNode('conjugate', TXMLArithtype);
  RegisterChildNode('factorial', TXMLArithtype);
  RegisterChildNode('arg', TXMLArithtype);
  RegisterChildNode('real', TXMLArithtype);
  RegisterChildNode('imaginary', TXMLArithtype);
  RegisterChildNode('floor', TXMLArithtype);
  RegisterChildNode('ceiling', TXMLArithtype);
  RegisterChildNode('quotient', TXMLArithtype);
  RegisterChildNode('divide', TXMLArithtype);
  RegisterChildNode('rem', TXMLArithtype);
  RegisterChildNode('minus', TXMLArithtype);
  RegisterChildNode('plus', TXMLArithtype);
  RegisterChildNode('times', TXMLArithtype);
  RegisterChildNode('power', TXMLArithtype);
  RegisterChildNode('root', TXMLArithtype);
  RegisterChildNode('max', TXMLArithtype);
  RegisterChildNode('min', TXMLArithtype);
  RegisterChildNode('gcd', TXMLArithtype);
  RegisterChildNode('lcm', TXMLArithtype);
  RegisterChildNode('sum', TXMLArithtype);
  RegisterChildNode('product', TXMLArithtype);
  RegisterChildNode('compose', TXMLFunctionstype);
  RegisterChildNode('domain', TXMLFunctionstype);
  RegisterChildNode('codomain', TXMLFunctionstype);
  RegisterChildNode('image', TXMLFunctionstype);
  RegisterChildNode('domainofapplication', TXMLDomainofapplicationtype);
  RegisterChildNode('ident', TXMLFunctionstype);
  RegisterChildNode('and', TXMLElementaryfunctionstype);
  RegisterChildNode('or', TXMLLogictype);
  RegisterChildNode('xor', TXMLLogictype);
  RegisterChildNode('not', TXMLLogictype);
  RegisterChildNode('exists', TXMLLogictype);
  RegisterChildNode('forall', TXMLLogictype);
  RegisterChildNode('implies', TXMLLogictype);
  RegisterChildNode('naturalnumbers', TXMLConstanttype);
  RegisterChildNode('primes', TXMLConstanttype);
  RegisterChildNode('integers', TXMLConstanttype);
  RegisterChildNode('rationals', TXMLConstanttype);
  RegisterChildNode('reals', TXMLConstanttype);
  RegisterChildNode('complexes', TXMLConstanttype);
  RegisterChildNode('emptyset', TXMLConstanttype);
  RegisterChildNode('exponentiale', TXMLConstanttype);
  RegisterChildNode('imaginaryi', TXMLConstanttype);
  RegisterChildNode('pi', TXMLConstanttype);
  RegisterChildNode('eulergamma', TXMLConstanttype);
  RegisterChildNode('true', TXMLConstanttype);
  RegisterChildNode('false', TXMLConstanttype);
  RegisterChildNode('infinity', TXMLConstanttype);
  RegisterChildNode('notanumber', TXMLConstanttype);
  RegisterChildNode('set', TXMLSettype);
  RegisterChildNode('list', TXMLListtype);
  RegisterChildNode('union', TXMLUniontype);
  RegisterChildNode('intersect', TXMLIntersecttype);
  RegisterChildNode('in', TXMLIntype);
  RegisterChildNode('notin', TXMLNotintype);
  RegisterChildNode('subset', TXMLSubsettype);
  RegisterChildNode('prsubset', TXMLPrsubsettype);
  RegisterChildNode('notsubset', TXMLNotsubsettype);
  RegisterChildNode('notprsubset', TXMLNotprsubsettype);
  RegisterChildNode('setdiff', TXMLSetdifftype);
  RegisterChildNode('card', TXMLCardtype);
  RegisterChildNode('cartesianproduct', TXMLCartesianproducttype);
  RegisterChildNode('eq', TXMLRelationstype);
  RegisterChildNode('neq', TXMLRelationstype);
  RegisterChildNode('leq', TXMLRelationstype);
  RegisterChildNode('lt', TXMLRelationstype);
  RegisterChildNode('geq', TXMLRelationstype);
  RegisterChildNode('gt', TXMLRelationstype);
  RegisterChildNode('equivalent', TXMLRelationstype);
  RegisterChildNode('approx', TXMLRelationstype);
  RegisterChildNode('factorof', TXMLRelationstype);
  RegisterChildNode('exp', TXMLElementaryfunctionstype);
  RegisterChildNode('ln', TXMLElementaryfunctionstype);
  RegisterChildNode('log', TXMLElementaryfunctionstype);
  RegisterChildNode('logbase', TXMLLogbasetype);
  RegisterChildNode('sin', TXMLElementaryfunctionstype);
  RegisterChildNode('cos', TXMLElementaryfunctionstype);
  RegisterChildNode('tan', TXMLElementaryfunctionstype);
  RegisterChildNode('sec', TXMLElementaryfunctionstype);
  RegisterChildNode('csc', TXMLElementaryfunctionstype);
  RegisterChildNode('cot', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsin', TXMLElementaryfunctionstype);
  RegisterChildNode('arccos', TXMLElementaryfunctionstype);
  RegisterChildNode('arctan', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsec', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsc', TXMLElementaryfunctionstype);
  RegisterChildNode('arccot', TXMLElementaryfunctionstype);
  RegisterChildNode('sinh', TXMLElementaryfunctionstype);
  RegisterChildNode('cosh', TXMLElementaryfunctionstype);
  RegisterChildNode('tanh', TXMLElementaryfunctionstype);
  RegisterChildNode('sech', TXMLElementaryfunctionstype);
  RegisterChildNode('csch', TXMLElementaryfunctionstype);
  RegisterChildNode('coth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccosh', TXMLElementaryfunctionstype);
  RegisterChildNode('arccoth', TXMLElementaryfunctionstype);
  RegisterChildNode('arccsch', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsech', TXMLElementaryfunctionstype);
  RegisterChildNode('arcsinh', TXMLElementaryfunctionstype);
  RegisterChildNode('arctanh', TXMLElementaryfunctionstype);
  RegisterChildNode('int', TXMLInttype);
  RegisterChildNode('diff', TXMLDifftype);
  RegisterChildNode('partialdiff', TXMLPartialdifftype);
  RegisterChildNode('limit', TXMLLimittype);
  RegisterChildNode('lowlimit', TXMLLowlimittype);
  RegisterChildNode('uplimit', TXMLUplimittype);
  RegisterChildNode('tendsto', TXMLTendstotype);
  RegisterChildNode('vector', TXMLVectortype);
  RegisterChildNode('matrix', TXMLMatrixtype);
  RegisterChildNode('determinant', TXMLDeterminanttype);
  RegisterChildNode('transpose', TXMLTransposetype);
  RegisterChildNode('selector', TXMLSelectortype);
  RegisterChildNode('vectorproduct', TXMLVectorproducttype);
  RegisterChildNode('scalarproduct', TXMLScalarproducttype);
  RegisterChildNode('outerproduct', TXMLOuterproducttype);
  RegisterChildNode('divergence', TXMLDivergencetype);
  RegisterChildNode('grad', TXMLGradtype);
  RegisterChildNode('curl', TXMLCurltype);
  RegisterChildNode('laplacian', TXMLLaplaciantype);
  RegisterChildNode('mean', TXMLMeantype);
  RegisterChildNode('sdev', TXMLSdevtype);
  RegisterChildNode('variance', TXMLVariancetype);
  RegisterChildNode('median', TXMLMediantype);
  RegisterChildNode('mode', TXMLModetype);
  RegisterChildNode('moment', TXMLMomenttype);
  RegisterChildNode('momentabout', TXMLMomentabouttype);
  RegisterChildNode('apply', TXMLApplytype);
  RegisterChildNode('interval', TXMLIntervaltype);
  RegisterChildNode('inverse', TXMLInversetype);
  RegisterChildNode('condition', TXMLConditiontype);
  RegisterChildNode('declare', TXMLDeclaretype);
  RegisterChildNode('lambda', TXMLLambdatype);
  RegisterChildNode('piecewise', TXMLPiecewisetype);
  RegisterChildNode('bvar', TXMLBvartype);
  RegisterChildNode('degree', TXMLDegreetype);
  RegisterChildNode('semantics', TXMLSemanticstype);
  inherited;
end;

function TXMLMathTextBaseType.Get_Mi: IXMLMitype;
begin
  Result := ChildNodes['mi'] as IXMLMitype;
end;

function TXMLMathTextBaseType.Get_Mo: IXMLMotype;
begin
  Result := ChildNodes['mo'] as IXMLMotype;
end;

function TXMLMathTextBaseType.Get_Mn: IXMLMntype;
begin
  Result := ChildNodes['mn'] as IXMLMntype;
end;

function TXMLMathTextBaseType.Get_Mtext: IXMLMtexttype;
begin
  Result := ChildNodes['mtext'] as IXMLMtexttype;
end;

function TXMLMathTextBaseType.Get_Ms: IXMLMstype;
begin
  Result := ChildNodes['ms'] as IXMLMstype;
end;

function TXMLMathTextBaseType.Get_Mrow: IXMLMrowtype;
begin
  Result := ChildNodes['mrow'] as IXMLMrowtype;
end;

function TXMLMathTextBaseType.Get_Mfrac: IXMLMfractype;
begin
  Result := ChildNodes['mfrac'] as IXMLMfractype;
end;

function TXMLMathTextBaseType.Get_Msqrt: IXMLMsqrttype;
begin
  Result := ChildNodes['msqrt'] as IXMLMsqrttype;
end;

function TXMLMathTextBaseType.Get_Mroot: IXMLMroottype;
begin
  Result := ChildNodes['mroot'] as IXMLMroottype;
end;

function TXMLMathTextBaseType.Get_Mpadded: IXMLMpaddedtype;
begin
  Result := ChildNodes['mpadded'] as IXMLMpaddedtype;
end;

function TXMLMathTextBaseType.Get_Mphantom: IXMLMphantomtype;
begin
  Result := ChildNodes['mphantom'] as IXMLMphantomtype;
end;

function TXMLMathTextBaseType.Get_Mfenced: IXMLMfencedtype;
begin
  Result := ChildNodes['mfenced'] as IXMLMfencedtype;
end;

function TXMLMathTextBaseType.Get_Menclose: IXMLMenclosetype;
begin
  Result := ChildNodes['menclose'] as IXMLMenclosetype;
end;

function TXMLMathTextBaseType.Get_Msub: IXMLMsubtype;
begin
  Result := ChildNodes['msub'] as IXMLMsubtype;
end;

function TXMLMathTextBaseType.Get_Msup: IXMLMsuptype;
begin
  Result := ChildNodes['msup'] as IXMLMsuptype;
end;

function TXMLMathTextBaseType.Get_Msubsup: IXMLMsubsuptype;
begin
  Result := ChildNodes['msubsup'] as IXMLMsubsuptype;
end;

function TXMLMathTextBaseType.Get_Munder: IXMLMundertype;
begin
  Result := ChildNodes['munder'] as IXMLMundertype;
end;

function TXMLMathTextBaseType.Get_Mover: IXMLMovertype;
begin
  Result := ChildNodes['mover'] as IXMLMovertype;
end;

function TXMLMathTextBaseType.Get_Munderover: IXMLMunderovertype;
begin
  Result := ChildNodes['munderover'] as IXMLMunderovertype;
end;

function TXMLMathTextBaseType.Get_Mmultiscripts: IXMLMmultiscriptstype;
begin
  Result := ChildNodes['mmultiscripts'] as IXMLMmultiscriptstype;
end;

function TXMLMathTextBaseType.Get_Mtable: IXMLMtabletype;
begin
  Result := ChildNodes['mtable'] as IXMLMtabletype;
end;

function TXMLMathTextBaseType.Get_Maligngroup: IXMLMaligngrouptype;
begin
  Result := ChildNodes['maligngroup'] as IXMLMaligngrouptype;
end;

function TXMLMathTextBaseType.Get_Malignmark: IXMLMalignmarktype;
begin
  Result := ChildNodes['malignmark'] as IXMLMalignmarktype;
end;

function TXMLMathTextBaseType.Get_Mspace: IXMLMspacetype;
begin
  Result := ChildNodes['mspace'] as IXMLMspacetype;
end;

procedure TXMLMathTextBaseType.AfterConstruction;
begin
  RegisterChildNode('mi', TXMLMitype);
  RegisterChildNode('mo', TXMLMotype);
  RegisterChildNode('mn', TXMLMntype);
  RegisterChildNode('mtext', TXMLMtexttype);
  RegisterChildNode('ms', TXMLMstype);
  RegisterChildNode('mrow', TXMLMrowtype);
  RegisterChildNode('mfrac', TXMLMfractype);
  RegisterChildNode('msqrt', TXMLMsqrttype);
  RegisterChildNode('mroot', TXMLMroottype);
  RegisterChildNode('mpadded', TXMLMpaddedtype);
  RegisterChildNode('mphantom', TXMLMphantomtype);
  RegisterChildNode('mfenced', TXMLMfencedtype);
  RegisterChildNode('menclose', TXMLMenclosetype);
  RegisterChildNode('msub', TXMLMsubtype);
  RegisterChildNode('msup', TXMLMsuptype);
  RegisterChildNode('msubsup', TXMLMsubsuptype);
  RegisterChildNode('munder', TXMLMundertype);
  RegisterChildNode('mover', TXMLMovertype);
  RegisterChildNode('munderover', TXMLMunderovertype);
  RegisterChildNode('mmultiscripts', TXMLMmultiscriptstype);
  RegisterChildNode('mtable', TXMLMtabletype);
  RegisterChildNode('maligngroup', TXMLMaligngrouptype);
  RegisterChildNode('malignmark', TXMLMalignmarktype);
  RegisterChildNode('mspace', TXMLMspacetype);
  RegisterChildNode('maction', TXMLMactiontype);
  RegisterChildNode('merror', TXMLMerrortype);
  RegisterChildNode('mstyle', TXMLMstyletype);

  inherited;
end;

function TXMLMathTextBaseType.Get_Maction: IXMLMactiontype;
begin
  Result := ChildNodes['maction'] as IXMLMactiontype;
end;

function TXMLMathTextBaseType.Get_Merror: IXMLMerrortype;
begin
  Result := ChildNodes['merror'] as IXMLMerrortype;
end;

function TXMLMathTextBaseType.Get_Mstyle: IXMLMstyletype;
begin
  Result := ChildNodes['mstyle'] as IXMLMstyletype;
end;

function TXMLMathBaseType.Get_Cn: IXMLCntype;
begin
  Result := ChildNodes['cn'] as IXMLCntype;
end;

function TXMLMathBaseType.Get_Ci: IXMLCitype;
begin
  Result := ChildNodes['ci'] as IXMLCitype;
end;

function TXMLMathBaseType.Get_Csymbol: IXMLCsymboltype;
begin
  Result := ChildNodes['csymbol'] as IXMLCsymboltype;
end;

function TXMLMathBaseType.Get_Abs: IXMLArithtype;
begin
  Result := ChildNodes['abs'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Conjugate: IXMLArithtype;
begin
  Result := ChildNodes['conjugate'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Factorial: IXMLArithtype;
begin
  Result := ChildNodes['factorial'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Arg: IXMLArithtype;
begin
  Result := ChildNodes['arg'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Real: IXMLArithtype;
begin
  Result := ChildNodes['real'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Imaginary: IXMLArithtype;
begin
  Result := ChildNodes['imaginary'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Floor: IXMLArithtype;
begin
  Result := ChildNodes['floor'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Ceiling: IXMLArithtype;
begin
  Result := ChildNodes['ceiling'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Quotient: IXMLArithtype;
begin
  Result := ChildNodes['quotient'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Divide: IXMLArithtype;
begin
  Result := ChildNodes['divide'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Rem: IXMLArithtype;
begin
  Result := ChildNodes['rem'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Minus: IXMLArithtype;
begin
  Result := ChildNodes['minus'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Plus: IXMLArithtype;
begin
  Result := ChildNodes['plus'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Times: IXMLArithtype;
begin
  Result := ChildNodes['times'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Power: IXMLArithtype;
begin
  Result := ChildNodes['power'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Root: IXMLArithtype;
begin
  Result := ChildNodes['root'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Max: IXMLArithtype;
begin
  Result := ChildNodes['max'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Min: IXMLArithtype;
begin
  Result := ChildNodes['min'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Gcd: IXMLArithtype;
begin
  Result := ChildNodes['gcd'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Lcm: IXMLArithtype;
begin
  Result := ChildNodes['lcm'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Sum: IXMLArithtype;
begin
  Result := ChildNodes['sum'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Product: IXMLArithtype;
begin
  Result := ChildNodes['product'] as IXMLArithtype;
end;

function TXMLMathBaseType.Get_Compose: IXMLFunctionstype;
begin
  Result := ChildNodes['compose'] as IXMLFunctionstype;
end;

function TXMLMathBaseType.Get_Domain: IXMLFunctionstype;
begin
  Result := ChildNodes['domain'] as IXMLFunctionstype;
end;

function TXMLMathBaseType.Get_Codomain: IXMLFunctionstype;
begin
  Result := ChildNodes['codomain'] as IXMLFunctionstype;
end;

function TXMLMathBaseType.Get_Image: IXMLFunctionstype;
begin
  Result := ChildNodes['image'] as IXMLFunctionstype;
end;

function TXMLMathBaseType.Get_Domainofapplication: IXMLDomainofapplicationtype;
begin
  Result := ChildNodes['domainofapplication'] as IXMLDomainofapplicationtype;
end;

function TXMLMathBaseType.Get_Ident: IXMLFunctionstype;
begin
  Result := ChildNodes['ident'] as IXMLFunctionstype;
end;

function TXMLMathBaseType.Get_And_: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['and'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Or_: IXMLLogictype;
begin
  Result := ChildNodes['or'] as IXMLLogictype;
end;

function TXMLMathBaseType.Get_Xor_: IXMLLogictype;
begin
  Result := ChildNodes['xor'] as IXMLLogictype;
end;

function TXMLMathBaseType.Get_Not_: IXMLLogictype;
begin
  Result := ChildNodes['not'] as IXMLLogictype;
end;

function TXMLMathBaseType.Get_Exists: IXMLLogictype;
begin
  Result := ChildNodes['exists'] as IXMLLogictype;
end;

function TXMLMathBaseType.Get_Forall: IXMLLogictype;
begin
  Result := ChildNodes['forall'] as IXMLLogictype;
end;

function TXMLMathBaseType.Get_Implies: IXMLLogictype;
begin
  Result := ChildNodes['implies'] as IXMLLogictype;
end;

function TXMLMathBaseType.Get_Naturalnumbers: IXMLConstanttype;
begin
  Result := ChildNodes['naturalnumbers'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Primes: IXMLConstanttype;
begin
  Result := ChildNodes['primes'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Integers: IXMLConstanttype;
begin
  Result := ChildNodes['integers'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Rationals: IXMLConstanttype;
begin
  Result := ChildNodes['rationals'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Reals: IXMLConstanttype;
begin
  Result := ChildNodes['reals'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Complexes: IXMLConstanttype;
begin
  Result := ChildNodes['complexes'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Emptyset: IXMLConstanttype;
begin
  Result := ChildNodes['emptyset'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Exponentiale: IXMLConstanttype;
begin
  Result := ChildNodes['exponentiale'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Imaginaryi: IXMLConstanttype;
begin
  Result := ChildNodes['imaginaryi'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Pi: IXMLConstanttype;
begin
  Result := ChildNodes['pi'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Eulergamma: IXMLConstanttype;
begin
  Result := ChildNodes['eulergamma'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_True: IXMLConstanttype;
begin
  Result := ChildNodes['true'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_False: IXMLConstanttype;
begin
  Result := ChildNodes['false'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Infinity: IXMLConstanttype;
begin
  Result := ChildNodes['infinity'] as IXMLConstanttype;
end;

function TXMLMathBaseType.Get_Notanumber: IXMLConstanttype;
begin
  Result := ChildNodes['notanumber'] as IXMLConstanttype;
end;
//
function TXMLMathBaseType.Get_Set_: IXMLSettype;
begin
  Result := ChildNodes['set'] as IXMLSettype;
end;

function TXMLMathBaseType.Get_List: IXMLListtype;
begin
  Result := ChildNodes['list'] as IXMLListtype;
end;

function TXMLMathBaseType.Get_Union: IXMLUniontype;
begin
  Result := ChildNodes['union'] as IXMLUniontype;
end;

function TXMLMathBaseType.Get_Intersect: IXMLIntersecttype;
begin
  Result := ChildNodes['intersect'] as IXMLIntersecttype;
end;

function TXMLMathBaseType.Get_In_: IXMLIntype;
begin
  Result := ChildNodes['in'] as IXMLIntype;
end;

function TXMLMathBaseType.Get_Notin: IXMLNotintype;
begin
  Result := ChildNodes['notin'] as IXMLNotintype;
end;

function TXMLMathBaseType.Get_Subset: IXMLSubsettype;
begin
  Result := ChildNodes['subset'] as IXMLSubsettype;
end;

function TXMLMathBaseType.Get_Prsubset: IXMLPrsubsettype;
begin
  Result := ChildNodes['prsubset'] as IXMLPrsubsettype;
end;

function TXMLMathBaseType.Get_Notsubset: IXMLNotsubsettype;
begin
  Result := ChildNodes['notsubset'] as IXMLNotsubsettype;
end;

function TXMLMathBaseType.Get_Notprsubset: IXMLNotprsubsettype;
begin
  Result := ChildNodes['notprsubset'] as IXMLNotprsubsettype;
end;

function TXMLMathBaseType.Get_Setdiff: IXMLSetdifftype;
begin
  Result := ChildNodes['setdiff'] as IXMLSetdifftype;
end;

function TXMLMathBaseType.Get_Card: IXMLCardtype;
begin
  Result := ChildNodes['card'] as IXMLCardtype;
end;

function TXMLMathBaseType.Get_Cartesianproduct: IXMLCartesianproducttype;
begin
  Result := ChildNodes['cartesianproduct'] as IXMLCartesianproducttype;
end;
//
function TXMLMathBaseType.Get_Eq: IXMLRelationstype;
begin
  Result := ChildNodes['eq'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Neq: IXMLRelationstype;
begin
  Result := ChildNodes['neq'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Leq: IXMLRelationstype;
begin
  Result := ChildNodes['leq'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Lt: IXMLRelationstype;
begin
  Result := ChildNodes['lt'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Geq: IXMLRelationstype;
begin
  Result := ChildNodes['geq'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Gt: IXMLRelationstype;
begin
  Result := ChildNodes['gt'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Equivalent: IXMLRelationstype;
begin
  Result := ChildNodes['equivalent'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Approx: IXMLRelationstype;
begin
  Result := ChildNodes['approx'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Factorof: IXMLRelationstype;
begin
  Result := ChildNodes['factorof'] as IXMLRelationstype;
end;

function TXMLMathBaseType.Get_Exp: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['exp'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Ln: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['ln'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Log: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['log'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Logbase: IXMLLogbasetype;
begin
  Result := ChildNodes['logbase'] as IXMLLogbasetype;
end;

function TXMLMathBaseType.Get_Sin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sin'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Cos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cos'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Tan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tan'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Sec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sec'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Csc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csc'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Cot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cot'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arcsin: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsin'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arccos: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccos'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arctan: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctan'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arcsec: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsec'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arccsc: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsc'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arccot: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccot'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Sinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sinh'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Cosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['cosh'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Tanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['tanh'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Sech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['sech'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Csch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['csch'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Coth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['coth'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arccosh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccosh'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arccoth: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccoth'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arccsch: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arccsch'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arcsech: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsech'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arcsinh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arcsinh'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Arctanh: IXMLElementaryfunctionstype;
begin
  Result := ChildNodes['arctanh'] as IXMLElementaryfunctionstype;
end;

function TXMLMathBaseType.Get_Int: IXMLInttype;
begin
  Result := ChildNodes['int'] as IXMLInttype;
end;

function TXMLMathBaseType.Get_Diff: IXMLDifftype;
begin
  Result := ChildNodes['diff'] as IXMLDifftype;
end;

function TXMLMathBaseType.Get_Partialdiff: IXMLPartialdifftype;
begin
  Result := ChildNodes['partialdiff'] as IXMLPartialdifftype;
end;

function TXMLMathBaseType.Get_Limit: IXMLLimittype;
begin
  Result := ChildNodes['limit'] as IXMLLimittype;
end;

function TXMLMathBaseType.Get_Lowlimit: IXMLLowlimittype;
begin
  Result := ChildNodes['lowlimit'] as IXMLLowlimittype;
end;

function TXMLMathBaseType.Get_Uplimit: IXMLUplimittype;
begin
  Result := ChildNodes['uplimit'] as IXMLUplimittype;
end;

function TXMLMathBaseType.Get_Tendsto: IXMLTendstotype;
begin
  Result := ChildNodes['tendsto'] as IXMLTendstotype;
end;

function TXMLMathBaseType.Get_Vector: IXMLVectortype;
begin
  Result := ChildNodes['vector'] as IXMLVectortype;
end;

function TXMLMathBaseType.Get_Matrix: IXMLMatrixtype;
begin
  Result := ChildNodes['matrix'] as IXMLMatrixtype;
end;

function TXMLMathBaseType.Get_Determinant: IXMLDeterminanttype;
begin
  Result := ChildNodes['determinant'] as IXMLDeterminanttype;
end;

function TXMLMathBaseType.Get_Transpose: IXMLTransposetype;
begin
  Result := ChildNodes['transpose'] as IXMLTransposetype;
end;

function TXMLMathBaseType.Get_Selector: IXMLSelectortype;
begin
  Result := ChildNodes['selector'] as IXMLSelectortype;
end;

function TXMLMathBaseType.Get_Vectorproduct: IXMLVectorproducttype;
begin
  Result := ChildNodes['vectorproduct'] as IXMLVectorproducttype;
end;

function TXMLMathBaseType.Get_Scalarproduct: IXMLScalarproducttype;
begin
  Result := ChildNodes['scalarproduct'] as IXMLScalarproducttype;
end;

function TXMLMathBaseType.Get_Outerproduct: IXMLOuterproducttype;
begin
  Result := ChildNodes['outerproduct'] as IXMLOuterproducttype;
end;

function TXMLMathBaseType.Get_Divergence: IXMLDivergencetype;
begin
  Result := ChildNodes['divergence'] as IXMLDivergencetype;
end;

function TXMLMathBaseType.Get_Grad: IXMLGradtype;
begin
  Result := ChildNodes['grad'] as IXMLGradtype;
end;

function TXMLMathBaseType.Get_Curl: IXMLCurltype;
begin
  Result := ChildNodes['curl'] as IXMLCurltype;
end;

function TXMLMathBaseType.Get_Laplacian: IXMLLaplaciantype;
begin
  Result := ChildNodes['laplacian'] as IXMLLaplaciantype;
end;

function TXMLMathBaseType.Get_Mean: IXMLMeantype;
begin
  Result := ChildNodes['mean'] as IXMLMeantype;
end;

function TXMLMathBaseType.Get_Sdev: IXMLSdevtype;
begin
  Result := ChildNodes['sdev'] as IXMLSdevtype;
end;

function TXMLMathBaseType.Get_Variance: IXMLVariancetype;
begin
  Result := ChildNodes['variance'] as IXMLVariancetype;
end;

function TXMLMathBaseType.Get_Median: IXMLMediantype;
begin
  Result := ChildNodes['median'] as IXMLMediantype;
end;

function TXMLMathBaseType.Get_Mode: IXMLModetype;
begin
  Result := ChildNodes['mode'] as IXMLModetype;
end;

function TXMLMathBaseType.Get_Moment: IXMLMomenttype;
begin
  Result := ChildNodes['moment'] as IXMLMomenttype;
end;

function TXMLMathBaseType.Get_Momentabout: IXMLMomentabouttype;
begin
  Result := ChildNodes['momentabout'] as IXMLMomentabouttype;
end;

function TXMLMathBaseType.Get_Apply: IXMLApplytype;
begin
  Result := ChildNodes['apply'] as IXMLApplytype;
end;

function TXMLMathBaseType.Get_Interval: IXMLIntervaltype;
begin
  Result := ChildNodes['interval'] as IXMLIntervaltype;
end;

function TXMLMathBaseType.Get_Inverse: IXMLInversetype;
begin
  Result := ChildNodes['inverse'] as IXMLInversetype;
end;

function TXMLMathBaseType.Get_Condition: IXMLConditiontype;
begin
  Result := ChildNodes['condition'] as IXMLConditiontype;
end;

function TXMLMathBaseType.Get_Declare: IXMLDeclaretype;
begin
  Result := ChildNodes['declare'] as IXMLDeclaretype;
end;

function TXMLMathBaseType.Get_Lambda: IXMLLambdatype;
begin
  Result := ChildNodes['lambda'] as IXMLLambdatype;
end;

function TXMLMathBaseType.Get_Piecewise: IXMLPiecewisetype;
begin
  Result := ChildNodes['piecewise'] as IXMLPiecewisetype;
end;

function TXMLMathBaseType.Get_Bvar: IXMLBvartype;
begin
  Result := ChildNodes['bvar'] as IXMLBvartype;
end;

function TXMLMathBaseType.Get_Degree: IXMLDegreetype;
begin
  Result := ChildNodes['degree'] as IXMLDegreetype;
end;

function TXMLMathBaseType.Get_Semantics: IXMLSemanticstype;
begin
  Result := ChildNodes['semantics'] as IXMLSemanticstype;
end;


end.
