{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit FMX.DAE.Schema.GLES;

interface
uses XML.XMLDoc, XML.XMLIntf, FMX.DAE.Schema;

type
{ TXMLProfile_gles2_type }

  TXMLProfile_gles2_type = class(TXMLNode, IXMLProfile_gles2_type)
  private
    FInclude: IXMLFx_include_typeList;
    FCode: IXMLFx_code_typeList;
    FNewparam: IXMLProfile_gles2_type_newparamList;
    FTechnique: IXMLProfile_gles2_type_techniqueList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_gles2_type }
    function Get_Id: UnicodeString;
    function Get_Language: UnicodeString;
    function Get_Platforms: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Include: IXMLFx_include_typeList;
    function Get_Code: IXMLFx_code_typeList;
    function Get_Newparam: IXMLProfile_gles2_type_newparamList;
    function Get_Technique: IXMLProfile_gles2_type_techniqueList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Language(Value: UnicodeString);
    procedure Set_Platforms(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_gles2_type_newparamList }

  TXMLProfile_gles2_type_newparamList = class(TXMLNodeCollection, IXMLProfile_gles2_type_newparamList)
  protected
    { IXMLProfile_gles2_type_newparamList }
    function Add: IXMLProfile_gles2_type_newparam;
    function Insert(const Index: Integer): IXMLProfile_gles2_type_newparam;

    function Get_Item(Index: Integer): IXMLProfile_gles2_type_newparam;
  end;

{ TXMLProfile_gles2_type_technique }

  TXMLProfile_gles2_type_technique = class(TXMLNode, IXMLProfile_gles2_type_technique)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FPass: IXMLGles2_pass_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_gles2_type_technique }
    function Get_Id: UnicodeString;
    function Get_Sid: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Pass: IXMLGles2_pass_typeList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_gles2_type_techniqueList }

  TXMLProfile_gles2_type_techniqueList = class(TXMLNodeCollection, IXMLProfile_gles2_type_techniqueList)
  protected
    { IXMLProfile_gles2_type_techniqueList }
    function Add: IXMLProfile_gles2_type_technique;
    function Insert(const Index: Integer): IXMLProfile_gles2_type_technique;

    function Get_Item(Index: Integer): IXMLProfile_gles2_type_technique;
  end;

{ TXMLGles2_pass_type }

  TXMLGles2_pass_type = class(TXMLNode, IXMLGles2_pass_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLGles2_pass_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_States: IXMLGles2_pass_type_states;
    function Get_Program_: IXMLGles2_program_type;
    function Get_Evaluate: IXMLGles2_pass_type_evaluate;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles2_pass_typeList }

  TXMLGles2_pass_typeList = class(TXMLNodeCollection, IXMLGles2_pass_typeList)
  protected
    { IXMLGles2_pass_typeList }
    function Add: IXMLGles2_pass_type;
    function Insert(const Index: Integer): IXMLGles2_pass_type;

    function Get_Item(Index: Integer): IXMLGles2_pass_type;
  end;

{ TXMLGles2_pass_type_states }

  TXMLGles2_pass_type_states = class(TXMLNode, IXMLGles2_pass_type_states)
  protected
    { IXMLGles2_pass_type_states }
    function Get_Blend_color: IXMLBlend_color;
    function Get_Blend_equation: IXMLBlend_equation;
    function Get_Blend_equation_separate: IXMLBlend_equation_separate;
    function Get_Blend_func: IXMLBlend_func;
    function Get_Blend_func_separate: IXMLBlend_func_separate;
    function Get_Color_mask: IXMLColor_mask;
    function Get_Cull_face: IXMLCull_face;
    function Get_Depth_func: IXMLDepth_func;
    function Get_Depth_mask: IXMLDepth_mask;
    function Get_Depth_range: IXMLDepth_range;
    function Get_Front_face: IXMLFront_face;
    function Get_Line_width: IXMLLine_width;
    function Get_Polygon_offset: IXMLPolygon_offset;
    function Get_Point_size: IXMLPoint_size;
    function Get_Sample_coverage: IXMLSample_coverage;
    function Get_Scissor: IXMLScissor;
    function Get_Stencil_func: IXMLStencil_func;
    function Get_Stencil_func_separate: IXMLStencil_func_separate;
    function Get_Stencil_mask: IXMLStencil_mask;
    function Get_Stencil_mask_separate: IXMLStencil_mask_separate;
    function Get_Stencil_op: IXMLStencil_op;
    function Get_Stencil_op_separate: IXMLStencil_op_separate;
    function Get_Blend_enable: IXMLBlend_enable;
    function Get_Cull_face_enable: IXMLCull_face_enable;
    function Get_Depth_test_enable: IXMLDepth_test_enable;
    function Get_Dither_enable: IXMLDither_enable;
    function Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
    function Get_Point_size_enable: IXMLPoint_size_enable;
    function Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
    function Get_Sample_coverage_enable: IXMLSample_coverage_enable;
    function Get_Scissor_test_enable: IXMLScissor_test_enable;
    function Get_Stencil_test_enable: IXMLStencil_test_enable;
  public
    procedure AfterConstruction; override;
  end;
  { TXMLFx_include_type }

  TXMLFx_include_type = class(TXMLNode, IXMLFx_include_type)
  protected
    { IXMLFx_include_type }
    function Get_Sid: UnicodeString;
    function Get_Url: UnicodeString;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Url(Value: UnicodeString);
  end;

{ TXMLFx_include_typeList }

  TXMLFx_include_typeList = class(TXMLNodeCollection, IXMLFx_include_typeList)
  protected
    { IXMLFx_include_typeList }
    function Add: IXMLFx_include_type;
    function Insert(const Index: Integer): IXMLFx_include_type;

    function Get_Item(Index: Integer): IXMLFx_include_type;
  end;

{ TXMLFx_code_type }

  TXMLFx_code_type = class(TXMLNode, IXMLFx_code_type)
  protected
    { IXMLFx_code_type }
    function Get_Sid: UnicodeString;
    procedure Set_Sid(Value: UnicodeString);
  end;

{ TXMLFx_code_typeList }

  TXMLFx_code_typeList = class(TXMLNodeCollection, IXMLFx_code_typeList)
  protected
    { IXMLFx_code_typeList }
    function Add: IXMLFx_code_type;
    function Insert(const Index: Integer): IXMLFx_code_type;

    function Get_Item(Index: Integer): IXMLFx_code_type;
  end;

{ TXMLGles2_newparam_type }

  TXMLGles2_newparam_type = class(TXMLNode, IXMLGles2_newparam_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
  protected
    { IXMLGles2_newparam_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Semantic: UnicodeString;
    function Get_Modifier: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bvec2: UnicodeString;
    function Get_Bvec3: UnicodeString;
    function Get_Bvec4: UnicodeString;
    function Get_Float: Double;
    function Get_Vec2: UnicodeString;
    function Get_Vec3: UnicodeString;
    function Get_Vec4: UnicodeString;
    function Get_Mat2: UnicodeString;
    function Get_Mat3: UnicodeString;
    function Get_Mat4: UnicodeString;
    function Get_Int: Int64;
    function Get_Ivec2: UnicodeString;
    function Get_Ivec3: UnicodeString;
    function Get_Ivec4: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Usertype: IXMLUsertype;
    function Get_Array_: IXMLArray_;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
    procedure Set_Modifier(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bvec2(Value: UnicodeString);
    procedure Set_Bvec3(Value: UnicodeString);
    procedure Set_Bvec4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Vec2(Value: UnicodeString);
    procedure Set_Vec3(Value: UnicodeString);
    procedure Set_Vec4(Value: UnicodeString);
    procedure Set_Mat2(Value: UnicodeString);
    procedure Set_Mat3(Value: UnicodeString);
    procedure Set_Mat4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Ivec2(Value: UnicodeString);
    procedure Set_Ivec3(Value: UnicodeString);
    procedure Set_Ivec4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLProfile_gles2_type_newparam }

  TXMLProfile_gles2_type_newparam = class(TXMLGles2_newparam_type, IXMLProfile_gles2_type_newparam)
  protected
    { IXMLProfile_gles2_type_newparam }
  end;
  { TXMLExtra_type }


  { TXMLFx_annotate_type }

  TXMLFx_annotate_type = class(TXMLNode, IXMLFx_annotate_type)
  protected
    { IXMLFx_annotate_type }
    function Get_Name: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_String_: UnicodeString;
    procedure Set_Name(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_String_(Value: UnicodeString);
  end;

{ TXMLFx_annotate_typeList }

  TXMLFx_annotate_typeList = class(TXMLNodeCollection, IXMLFx_annotate_typeList)
  protected
    { IXMLFx_annotate_typeList }
    function Add: IXMLFx_annotate_type;
    function Insert(const Index: Integer): IXMLFx_annotate_type;

    function Get_Item(Index: Integer): IXMLFx_annotate_type;
  end;

{ TXMLFx_newparam_type }

  TXMLFx_newparam_type = class(TXMLNode, IXMLFx_newparam_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
  protected
    { IXMLFx_newparam_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Semantic: UnicodeString;
    function Get_Modifier: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x1: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float2x3: UnicodeString;
    function Get_Float2x4: UnicodeString;
    function Get_Float3x1: UnicodeString;
    function Get_Float3x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float3x4: UnicodeString;
    function Get_Float4x1: UnicodeString;
    function Get_Float4x2: UnicodeString;
    function Get_Float4x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Enum: UnicodeString;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
    procedure Set_Modifier(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x1(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float2x3(Value: UnicodeString);
    procedure Set_Float2x4(Value: UnicodeString);
    procedure Set_Float3x1(Value: UnicodeString);
    procedure Set_Float3x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float3x4(Value: UnicodeString);
    procedure Set_Float4x1(Value: UnicodeString);
    procedure Set_Float4x2(Value: UnicodeString);
    procedure Set_Float4x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_newparam_typeList }

  TXMLFx_newparam_typeList = class(TXMLNodeCollection, IXMLFx_newparam_typeList)
  protected
    { IXMLFx_newparam_typeList }
    function Add: IXMLFx_newparam_type;
    function Insert(const Index: Integer): IXMLFx_newparam_type;

    function Get_Item(Index: Integer): IXMLFx_newparam_type;
  end;

{ TXMLFx_sampler_type }

  TXMLFx_sampler_type = class(TXMLNode, IXMLFx_sampler_type)
  private
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLFx_sampler_type }
    function Get_Instance_image: IXMLInstance_image_type;
    function Get_Wrap_s: UnicodeString;
    function Get_Wrap_t: UnicodeString;
    function Get_Wrap_p: UnicodeString;
    function Get_Minfilter: UnicodeString;
    function Get_Magfilter: UnicodeString;
    function Get_Mipfilter: UnicodeString;
    function Get_Border_color: UnicodeString;
    function Get_Mip_max_level: Byte;
    function Get_Mip_min_level: Byte;
    function Get_Mip_bias: Single;
    function Get_Max_anisotropy: LongWord;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Wrap_s(Value: UnicodeString);
    procedure Set_Wrap_t(Value: UnicodeString);
    procedure Set_Wrap_p(Value: UnicodeString);
    procedure Set_Minfilter(Value: UnicodeString);
    procedure Set_Magfilter(Value: UnicodeString);
    procedure Set_Mipfilter(Value: UnicodeString);
    procedure Set_Border_color(Value: UnicodeString);
    procedure Set_Mip_max_level(Value: Byte);
    procedure Set_Mip_min_level(Value: Byte);
    procedure Set_Mip_bias(Value: Single);
    procedure Set_Max_anisotropy(Value: LongWord);
  public
    procedure AfterConstruction; override;
  end;


{ TXMLFx_sampler1D_type }

  TXMLFx_sampler1D_type = class(TXMLFx_sampler_type, IXMLFx_sampler1D_type)
  protected
    { IXMLFx_sampler1D_type }
  end;

{ TXMLFx_sampler2D_type }

  TXMLFx_sampler2D_type = class(TXMLFx_sampler_type, IXMLFx_sampler2D_type)
  protected
    { IXMLFx_sampler2D_type }
  end;

{ TXMLFx_sampler3D_type }

  TXMLFx_sampler3D_type = class(TXMLFx_sampler_type, IXMLFx_sampler3D_type)
  protected
    { IXMLFx_sampler3D_type }
  end;

{ TXMLFx_samplerCUBE_type }

  TXMLFx_samplerCUBE_type = class(TXMLFx_sampler_type, IXMLFx_samplerCUBE_type)
  protected
    { IXMLFx_samplerCUBE_type }
  end;

{ TXMLFx_samplerRECT_type }

  TXMLFx_samplerRECT_type = class(TXMLFx_sampler_type, IXMLFx_samplerRECT_type)
  protected
    { IXMLFx_samplerRECT_type }
  end;

{ TXMLFx_samplerDEPTH_type }

  TXMLFx_samplerDEPTH_type = class(TXMLFx_sampler_type, IXMLFx_samplerDEPTH_type)
  protected
    { IXMLFx_samplerDEPTH_type }
  end;

  { TXMLUsertype }

  TXMLUsertype = class(TXMLNodeCollection, IXMLUsertype)
  protected
    { IXMLUsertype }
    function Get_Typename: UnicodeString;
    function Get_Setparam(Index: Integer): IXMLUsertype_setparam;
    procedure Set_Typename(Value: UnicodeString);
    function Add: IXMLUsertype_setparam;
    function Insert(const Index: Integer): IXMLUsertype_setparam;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLUsertype_setparam }

  TXMLUsertype_setparam = class(TXMLNode, IXMLUsertype_setparam)
  protected
    { IXMLUsertype_setparam }
    function Get_Ref: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bvec2: UnicodeString;
    function Get_Bvec3: UnicodeString;
    function Get_Bvec4: UnicodeString;
    function Get_Float: Double;
    function Get_Vec2: UnicodeString;
    function Get_Vec3: UnicodeString;
    function Get_Vec4: UnicodeString;
    function Get_Mat2: UnicodeString;
    function Get_Mat3: UnicodeString;
    function Get_Mat4: UnicodeString;
    function Get_Int: Int64;
    function Get_Ivec2: UnicodeString;
    function Get_Ivec3: UnicodeString;
    function Get_Ivec4: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Usertype: IXMLUsertype;
    function Get_Array_: IXMLArray_;
    procedure Set_Ref(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bvec2(Value: UnicodeString);
    procedure Set_Bvec3(Value: UnicodeString);
    procedure Set_Bvec4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Vec2(Value: UnicodeString);
    procedure Set_Vec3(Value: UnicodeString);
    procedure Set_Vec4(Value: UnicodeString);
    procedure Set_Mat2(Value: UnicodeString);
    procedure Set_Mat3(Value: UnicodeString);
    procedure Set_Mat4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Ivec2(Value: UnicodeString);
    procedure Set_Ivec3(Value: UnicodeString);
    procedure Set_Ivec4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLArray_ }

  TXMLArray_ = class(TXMLNode, IXMLArray_)
  protected
    { IXMLArray_ }
    function Get_Length: LongWord;
    function Get_Bool: Boolean;
    function Get_Bvec2: UnicodeString;
    function Get_Bvec3: UnicodeString;
    function Get_Bvec4: UnicodeString;
    function Get_Float: Double;
    function Get_Vec2: UnicodeString;
    function Get_Vec3: UnicodeString;
    function Get_Vec4: UnicodeString;
    function Get_Mat2: UnicodeString;
    function Get_Mat3: UnicodeString;
    function Get_Mat4: UnicodeString;
    function Get_Int: Int64;
    function Get_Ivec2: UnicodeString;
    function Get_Ivec3: UnicodeString;
    function Get_Ivec4: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Usertype: IXMLUsertype;
    function Get_Array_: IXMLArray_;
    procedure Set_Length(Value: LongWord);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bvec2(Value: UnicodeString);
    procedure Set_Bvec3(Value: UnicodeString);
    procedure Set_Bvec4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Vec2(Value: UnicodeString);
    procedure Set_Vec3(Value: UnicodeString);
    procedure Set_Vec4(Value: UnicodeString);
    procedure Set_Mat2(Value: UnicodeString);
    procedure Set_Mat3(Value: UnicodeString);
    procedure Set_Mat4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Ivec2(Value: UnicodeString);
    procedure Set_Ivec3(Value: UnicodeString);
    procedure Set_Ivec4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBlend_color }

  TXMLBlend_color = class(TXMLNode, IXMLBlend_color)
  protected
    { IXMLBlend_color }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_equation }

  TXMLBlend_equation = class(TXMLNode, IXMLBlend_equation)
  protected
    { IXMLBlend_equation }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_equation_separate }

  TXMLBlend_equation_separate = class(TXMLNode, IXMLBlend_equation_separate)
  protected
    { IXMLBlend_equation_separate }
    function Get_Rgb: IXMLBlend_equation_separate_rgb;
    function Get_Alpha: IXMLBlend_equation_separate_alpha;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBlend_equation_separate_rgb }

  TXMLBlend_equation_separate_rgb = class(TXMLNode, IXMLBlend_equation_separate_rgb)
  protected
    { IXMLBlend_equation_separate_rgb }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_equation_separate_alpha }

  TXMLBlend_equation_separate_alpha = class(TXMLNode, IXMLBlend_equation_separate_alpha)
  protected
    { IXMLBlend_equation_separate_alpha }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_func }

  TXMLBlend_func = class(TXMLNode, IXMLBlend_func)
  protected
    { IXMLBlend_func }
    function Get_Src: IXMLBlend_func_src;
    function Get_Dest: IXMLBlend_func_dest;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBlend_func_src }

  TXMLBlend_func_src = class(TXMLNode, IXMLBlend_func_src)
  protected
    { IXMLBlend_func_src }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_func_dest }

  TXMLBlend_func_dest = class(TXMLNode, IXMLBlend_func_dest)
  protected
    { IXMLBlend_func_dest }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_func_separate }

  TXMLBlend_func_separate = class(TXMLNode, IXMLBlend_func_separate)
  protected
    { IXMLBlend_func_separate }
    function Get_Src_rgb: IXMLBlend_func_separate_src_rgb;
    function Get_Dest_rgb: IXMLBlend_func_separate_dest_rgb;
    function Get_Src_alpha: IXMLBlend_func_separate_src_alpha;
    function Get_Dest_alpha: IXMLBlend_func_separate_dest_alpha;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLBlend_func_separate_src_rgb }

  TXMLBlend_func_separate_src_rgb = class(TXMLNode, IXMLBlend_func_separate_src_rgb)
  protected
    { IXMLBlend_func_separate_src_rgb }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_func_separate_dest_rgb }

  TXMLBlend_func_separate_dest_rgb = class(TXMLNode, IXMLBlend_func_separate_dest_rgb)
  protected
    { IXMLBlend_func_separate_dest_rgb }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_func_separate_src_alpha }

  TXMLBlend_func_separate_src_alpha = class(TXMLNode, IXMLBlend_func_separate_src_alpha)
  protected
    { IXMLBlend_func_separate_src_alpha }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_func_separate_dest_alpha }

  TXMLBlend_func_separate_dest_alpha = class(TXMLNode, IXMLBlend_func_separate_dest_alpha)
  protected
    { IXMLBlend_func_separate_dest_alpha }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLColor_mask }

  TXMLColor_mask = class(TXMLNode, IXMLColor_mask)
  protected
    { IXMLColor_mask }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLCull_face }

  TXMLCull_face = class(TXMLNode, IXMLCull_face)
  protected
    { IXMLCull_face }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDepth_func }

  TXMLDepth_func = class(TXMLNode, IXMLDepth_func)
  protected
    { IXMLDepth_func }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDepth_mask }

  TXMLDepth_mask = class(TXMLNode, IXMLDepth_mask)
  protected
    { IXMLDepth_mask }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDepth_range }

  TXMLDepth_range = class(TXMLNode, IXMLDepth_range)
  protected
    { IXMLDepth_range }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFront_face }

  TXMLFront_face = class(TXMLNode, IXMLFront_face)
  protected
    { IXMLFront_face }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLine_width }

  TXMLLine_width = class(TXMLNode, IXMLLine_width)
  protected
    { IXMLLine_width }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_offset }

  TXMLPolygon_offset = class(TXMLNode, IXMLPolygon_offset)
  protected
    { IXMLPolygon_offset }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_size }

  TXMLPoint_size = class(TXMLNode, IXMLPoint_size)
  protected
    { IXMLPoint_size }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLSample_coverage }

  TXMLSample_coverage = class(TXMLNode, IXMLSample_coverage)
  protected
    { IXMLSample_coverage }
    function Get_Value: IXMLSample_coverage_value;
    function Get_Invert: IXMLSample_coverage_invert;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSample_coverage_value }

  TXMLSample_coverage_value = class(TXMLNode, IXMLSample_coverage_value)
  protected
    { IXMLSample_coverage_value }
    function Get_Value: Single;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Single);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLSample_coverage_invert }

  TXMLSample_coverage_invert = class(TXMLNode, IXMLSample_coverage_invert)
  protected
    { IXMLSample_coverage_invert }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLScissor }

  TXMLScissor = class(TXMLNode, IXMLScissor)
  protected
    { IXMLScissor }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func }

  TXMLStencil_func = class(TXMLNode, IXMLStencil_func)
  protected
    { IXMLStencil_func }
    function Get_Func: IXMLStencil_func_func;
    function Get_Ref: IXMLStencil_func_ref;
    function Get_Mask: IXMLStencil_func_mask;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStencil_func_func }

  TXMLStencil_func_func = class(TXMLNode, IXMLStencil_func_func)
  protected
    { IXMLStencil_func_func }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func_ref }

  TXMLStencil_func_ref = class(TXMLNode, IXMLStencil_func_ref)
  protected
    { IXMLStencil_func_ref }
    function Get_Value: Byte;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Byte);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func_mask }

  TXMLStencil_func_mask = class(TXMLNode, IXMLStencil_func_mask)
  protected
    { IXMLStencil_func_mask }
    function Get_Value: Byte;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Byte);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func_separate }

  TXMLStencil_func_separate = class(TXMLNode, IXMLStencil_func_separate)
  protected
    { IXMLStencil_func_separate }
    function Get_Front: IXMLStencil_func_separate_front;
    function Get_Back: IXMLStencil_func_separate_back;
    function Get_Ref: IXMLStencil_func_separate_ref;
    function Get_Mask: IXMLStencil_func_separate_mask;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStencil_func_separate_front }

  TXMLStencil_func_separate_front = class(TXMLNode, IXMLStencil_func_separate_front)
  protected
    { IXMLStencil_func_separate_front }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func_separate_back }

  TXMLStencil_func_separate_back = class(TXMLNode, IXMLStencil_func_separate_back)
  protected
    { IXMLStencil_func_separate_back }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func_separate_ref }

  TXMLStencil_func_separate_ref = class(TXMLNode, IXMLStencil_func_separate_ref)
  protected
    { IXMLStencil_func_separate_ref }
    function Get_Value: Byte;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Byte);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_func_separate_mask }

  TXMLStencil_func_separate_mask = class(TXMLNode, IXMLStencil_func_separate_mask)
  protected
    { IXMLStencil_func_separate_mask }
    function Get_Value: Byte;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Byte);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_mask }

  TXMLStencil_mask = class(TXMLNode, IXMLStencil_mask)
  protected
    { IXMLStencil_mask }
    function Get_Value: Int64;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Int64);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_mask_separate }

  TXMLStencil_mask_separate = class(TXMLNode, IXMLStencil_mask_separate)
  protected
    { IXMLStencil_mask_separate }
    function Get_Face: IXMLStencil_mask_separate_face;
    function Get_Mask: IXMLStencil_mask_separate_mask;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStencil_mask_separate_face }

  TXMLStencil_mask_separate_face = class(TXMLNode, IXMLStencil_mask_separate_face)
  protected
    { IXMLStencil_mask_separate_face }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_mask_separate_mask }

  TXMLStencil_mask_separate_mask = class(TXMLNode, IXMLStencil_mask_separate_mask)
  protected
    { IXMLStencil_mask_separate_mask }
    function Get_Value: Byte;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Byte);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op }

  TXMLStencil_op = class(TXMLNode, IXMLStencil_op)
  protected
    { IXMLStencil_op }
    function Get_Fail: IXMLStencil_op_fail;
    function Get_Zfail: IXMLStencil_op_zfail;
    function Get_Zpass: IXMLStencil_op_zpass;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStencil_op_fail }

  TXMLStencil_op_fail = class(TXMLNode, IXMLStencil_op_fail)
  protected
    { IXMLStencil_op_fail }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op_zfail }

  TXMLStencil_op_zfail = class(TXMLNode, IXMLStencil_op_zfail)
  protected
    { IXMLStencil_op_zfail }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op_zpass }

  TXMLStencil_op_zpass = class(TXMLNode, IXMLStencil_op_zpass)
  protected
    { IXMLStencil_op_zpass }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op_separate }

  TXMLStencil_op_separate = class(TXMLNode, IXMLStencil_op_separate)
  protected
    { IXMLStencil_op_separate }
    function Get_Face: IXMLStencil_op_separate_face;
    function Get_Fail: IXMLStencil_op_separate_fail;
    function Get_Zfail: IXMLStencil_op_separate_zfail;
    function Get_Zpass: IXMLStencil_op_separate_zpass;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLStencil_op_separate_face }

  TXMLStencil_op_separate_face = class(TXMLNode, IXMLStencil_op_separate_face)
  protected
    { IXMLStencil_op_separate_face }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op_separate_fail }

  TXMLStencil_op_separate_fail = class(TXMLNode, IXMLStencil_op_separate_fail)
  protected
    { IXMLStencil_op_separate_fail }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op_separate_zfail }

  TXMLStencil_op_separate_zfail = class(TXMLNode, IXMLStencil_op_separate_zfail)
  protected
    { IXMLStencil_op_separate_zfail }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_op_separate_zpass }

  TXMLStencil_op_separate_zpass = class(TXMLNode, IXMLStencil_op_separate_zpass)
  protected
    { IXMLStencil_op_separate_zpass }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLBlend_enable }

  TXMLBlend_enable = class(TXMLNode, IXMLBlend_enable)
  protected
    { IXMLBlend_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLCull_face_enable }

  TXMLCull_face_enable = class(TXMLNode, IXMLCull_face_enable)
  protected
    { IXMLCull_face_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDepth_test_enable }

  TXMLDepth_test_enable = class(TXMLNode, IXMLDepth_test_enable)
  protected
    { IXMLDepth_test_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDither_enable }

  TXMLDither_enable = class(TXMLNode, IXMLDither_enable)
  protected
    { IXMLDither_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_offset_fill_enable }

  TXMLPolygon_offset_fill_enable = class(TXMLNode, IXMLPolygon_offset_fill_enable)
  protected
    { IXMLPolygon_offset_fill_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_size_enable }

  TXMLPoint_size_enable = class(TXMLNode, IXMLPoint_size_enable)
  protected
    { IXMLPoint_size_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLSample_alpha_to_coverage_enable }

  TXMLSample_alpha_to_coverage_enable = class(TXMLNode, IXMLSample_alpha_to_coverage_enable)
  protected
    { IXMLSample_alpha_to_coverage_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLSample_coverage_enable }

  TXMLSample_coverage_enable = class(TXMLNode, IXMLSample_coverage_enable)
  protected
    { IXMLSample_coverage_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLScissor_test_enable }

  TXMLScissor_test_enable = class(TXMLNode, IXMLScissor_test_enable)
  protected
    { IXMLScissor_test_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLStencil_test_enable }

  TXMLStencil_test_enable = class(TXMLNode, IXMLStencil_test_enable)
  protected
    { IXMLStencil_test_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLGles2_program_type }

  TXMLGles2_program_type = class(TXMLNode, IXMLGles2_program_type)
  private
    FShader: IXMLGles2_shader_typeList;
    FLinker: IXMLFx_target_typeList;
    FBind_attribute: IXMLGles2_program_type_bind_attributeList;
    FBind_uniform: IXMLGles2_program_type_bind_uniformList;
  protected
    { IXMLGles2_program_type }
    function Get_Shader: IXMLGles2_shader_typeList;
    function Get_Linker: IXMLFx_target_typeList;
    function Get_Bind_attribute: IXMLGles2_program_type_bind_attributeList;
    function Get_Bind_uniform: IXMLGles2_program_type_bind_uniformList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles2_shader_type }

  TXMLGles2_shader_type = class(TXMLNode, IXMLGles2_shader_type)
  private
    FCompiler: IXMLFx_target_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLGles2_shader_type }
    function Get_Stage: UnicodeString;
    function Get_Sources: IXMLGles2_shader_type_sources;
    function Get_Compiler: IXMLFx_target_typeList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Stage(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles2_shader_typeList }

  TXMLGles2_shader_typeList = class(TXMLNodeCollection, IXMLGles2_shader_typeList)
  protected
    { IXMLGles2_shader_typeList }
    function Add: IXMLGles2_shader_type;
    function Insert(const Index: Integer): IXMLGles2_shader_type;

    function Get_Item(Index: Integer): IXMLGles2_shader_type;
  end;

{ TXMLFx_sources_type }

  TXMLFx_sources_type = class(TXMLNode, IXMLFx_sources_type)
  private
    FInline_: IXMLString_List;
    FImport: IXMLFx_sources_type_importList;
  protected
    { IXMLFx_sources_type }
    function Get_Inline_: IXMLString_List;
    function Get_Import: IXMLFx_sources_type_importList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_sources_type_import }

  TXMLFx_sources_type_import = class(TXMLNode, IXMLFx_sources_type_import)
  protected
    { IXMLFx_sources_type_import }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLFx_sources_type_importList }

  TXMLFx_sources_type_importList = class(TXMLNodeCollection, IXMLFx_sources_type_importList)
  protected
    { IXMLFx_sources_type_importList }
    function Add: IXMLFx_sources_type_import;
    function Insert(const Index: Integer): IXMLFx_sources_type_import;

    function Get_Item(Index: Integer): IXMLFx_sources_type_import;
  end;

{ TXMLGles2_shader_type_sources }

  TXMLGles2_shader_type_sources = class(TXMLFx_sources_type, IXMLGles2_shader_type_sources)
  protected
    { IXMLGles2_shader_type_sources }
    function Get_Entry: UnicodeString;
    procedure Set_Entry(Value: UnicodeString);
  end;

{ TXMLFx_target_type }

  TXMLFx_target_type = class(TXMLNode, IXMLFx_target_type)
  protected
    { IXMLFx_target_type }
    function Get_Platform: UnicodeString;
    function Get_Target: UnicodeString;
    function Get_Options: UnicodeString;
    function Get_Binary: IXMLFx_target_type_binary;
    procedure Set_Platform(Value: UnicodeString);
    procedure Set_Target(Value: UnicodeString);
    procedure Set_Options(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_target_typeList }

  TXMLFx_target_typeList = class(TXMLNodeCollection, IXMLFx_target_typeList)
  protected
    { IXMLFx_target_typeList }
    function Add: IXMLFx_target_type;
    function Insert(const Index: Integer): IXMLFx_target_type;

    function Get_Item(Index: Integer): IXMLFx_target_type;
  end;

{ TXMLFx_target_type_binary }

  TXMLFx_target_type_binary = class(TXMLNode, IXMLFx_target_type_binary)
  protected
    { IXMLFx_target_type_binary }
    function Get_Ref: UnicodeString;
    function Get_Hex: IXMLFx_target_type_binary_hex;
    procedure Set_Ref(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_target_type_binary_hex }

  TXMLFx_target_type_binary_hex = class(TXMLNode, IXMLFx_target_type_binary_hex)
  protected
    { IXMLFx_target_type_binary_hex }
    function Get_Format: UnicodeString;
    procedure Set_Format(Value: UnicodeString);
  end;

{ TXMLGles2_program_type_bind_attribute }

  TXMLGles2_program_type_bind_attribute = class(TXMLNode, IXMLGles2_program_type_bind_attribute)
  protected
    { IXMLGles2_program_type_bind_attribute }
    function Get_Symbol: UnicodeString;
    function Get_Semantic: UnicodeString;
    procedure Set_Symbol(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
  end;

{ TXMLGles2_program_type_bind_attributeList }

  TXMLGles2_program_type_bind_attributeList = class(TXMLNodeCollection, IXMLGles2_program_type_bind_attributeList)
  protected
    { IXMLGles2_program_type_bind_attributeList }
    function Add: IXMLGles2_program_type_bind_attribute;
    function Insert(const Index: Integer): IXMLGles2_program_type_bind_attribute;

    function Get_Item(Index: Integer): IXMLGles2_program_type_bind_attribute;
  end;

{ TXMLGles2_program_type_bind_uniform }

  TXMLGles2_program_type_bind_uniform = class(TXMLNode, IXMLGles2_program_type_bind_uniform)
  protected
    { IXMLGles2_program_type_bind_uniform }
    function Get_Symbol: UnicodeString;
    function Get_Param: IXMLGles2_program_type_bind_uniform_param;
    function Get_Bool: Boolean;
    function Get_Bvec2: UnicodeString;
    function Get_Bvec3: UnicodeString;
    function Get_Bvec4: UnicodeString;
    function Get_Float: Double;
    function Get_Vec2: UnicodeString;
    function Get_Vec3: UnicodeString;
    function Get_Vec4: UnicodeString;
    function Get_Mat2: UnicodeString;
    function Get_Mat3: UnicodeString;
    function Get_Mat4: UnicodeString;
    function Get_Int: Int64;
    function Get_Ivec2: UnicodeString;
    function Get_Ivec3: UnicodeString;
    function Get_Ivec4: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Usertype: IXMLUsertype;
    function Get_Array_: IXMLArray_;
    procedure Set_Symbol(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bvec2(Value: UnicodeString);
    procedure Set_Bvec3(Value: UnicodeString);
    procedure Set_Bvec4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Vec2(Value: UnicodeString);
    procedure Set_Vec3(Value: UnicodeString);
    procedure Set_Vec4(Value: UnicodeString);
    procedure Set_Mat2(Value: UnicodeString);
    procedure Set_Mat3(Value: UnicodeString);
    procedure Set_Mat4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Ivec2(Value: UnicodeString);
    procedure Set_Ivec3(Value: UnicodeString);
    procedure Set_Ivec4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles2_program_type_bind_uniformList }

  TXMLGles2_program_type_bind_uniformList = class(TXMLNodeCollection, IXMLGles2_program_type_bind_uniformList)
  protected
    { IXMLGles2_program_type_bind_uniformList }
    function Add: IXMLGles2_program_type_bind_uniform;
    function Insert(const Index: Integer): IXMLGles2_program_type_bind_uniform;

    function Get_Item(Index: Integer): IXMLGles2_program_type_bind_uniform;
  end;

{ TXMLGles2_program_type_bind_uniform_param }

  TXMLGles2_program_type_bind_uniform_param = class(TXMLNode, IXMLGles2_program_type_bind_uniform_param)
  protected
    { IXMLGles2_program_type_bind_uniform_param }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLGles2_pass_type_evaluate }

  TXMLGles2_pass_type_evaluate = class(TXMLNode, IXMLGles2_pass_type_evaluate)
  private
    FColor_target: IXMLFx_colortarget_typeList;
    FDepth_target: IXMLFx_depthtarget_typeList;
    FStencil_target: IXMLFx_stenciltarget_typeList;
    FColor_clear: IXMLFx_clearcolor_typeList;
    FStencil_clear: IXMLFx_clearstencil_typeList;
    FDepth_clear: IXMLFx_cleardepth_typeList;
  protected
    { IXMLGles2_pass_type_evaluate }
    function Get_Color_target: IXMLFx_colortarget_typeList;
    function Get_Depth_target: IXMLFx_depthtarget_typeList;
    function Get_Stencil_target: IXMLFx_stenciltarget_typeList;
    function Get_Color_clear: IXMLFx_clearcolor_typeList;
    function Get_Stencil_clear: IXMLFx_clearstencil_typeList;
    function Get_Depth_clear: IXMLFx_cleardepth_typeList;
    function Get_Draw: UnicodeString;
    procedure Set_Draw(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_rendertarget_type }

  TXMLFx_rendertarget_type = class(TXMLNode, IXMLFx_rendertarget_type)
  protected
    { IXMLFx_rendertarget_type }
    function Get_Index: LongWord;
    function Get_Mip: LongWord;
    function Get_Face: UnicodeString;
    function Get_Slice: LongWord;
    function Get_Param: IXMLFx_rendertarget_type_param;
    function Get_Instance_image: IXMLInstance_image_type;
    procedure Set_Index(Value: LongWord);
    procedure Set_Mip(Value: LongWord);
    procedure Set_Face(Value: UnicodeString);
    procedure Set_Slice(Value: LongWord);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_rendertarget_type_param }

  TXMLFx_rendertarget_type_param = class(TXMLNode, IXMLFx_rendertarget_type_param)
  protected
    { IXMLFx_rendertarget_type_param }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLFx_colortarget_type }

  TXMLFx_colortarget_type = class(TXMLFx_rendertarget_type, IXMLFx_colortarget_type)
  protected
    { IXMLFx_colortarget_type }
  end;

{ TXMLFx_colortarget_typeList }

  TXMLFx_colortarget_typeList = class(TXMLNodeCollection, IXMLFx_colortarget_typeList)
  protected
    { IXMLFx_colortarget_typeList }
    function Add: IXMLFx_colortarget_type;
    function Insert(const Index: Integer): IXMLFx_colortarget_type;

    function Get_Item(Index: Integer): IXMLFx_colortarget_type;
  end;

{ TXMLFx_depthtarget_type }

  TXMLFx_depthtarget_type = class(TXMLFx_rendertarget_type, IXMLFx_depthtarget_type)
  protected
    { IXMLFx_depthtarget_type }
  end;

{ TXMLFx_depthtarget_typeList }

  TXMLFx_depthtarget_typeList = class(TXMLNodeCollection, IXMLFx_depthtarget_typeList)
  protected
    { IXMLFx_depthtarget_typeList }
    function Add: IXMLFx_depthtarget_type;
    function Insert(const Index: Integer): IXMLFx_depthtarget_type;

    function Get_Item(Index: Integer): IXMLFx_depthtarget_type;
  end;

{ TXMLFx_stenciltarget_type }

  TXMLFx_stenciltarget_type = class(TXMLFx_rendertarget_type, IXMLFx_stenciltarget_type)
  protected
    { IXMLFx_stenciltarget_type }
  end;

{ TXMLFx_stenciltarget_typeList }

  TXMLFx_stenciltarget_typeList = class(TXMLNodeCollection, IXMLFx_stenciltarget_typeList)
  protected
    { IXMLFx_stenciltarget_typeList }
    function Add: IXMLFx_stenciltarget_type;
    function Insert(const Index: Integer): IXMLFx_stenciltarget_type;

    function Get_Item(Index: Integer): IXMLFx_stenciltarget_type;
  end;

{ TXMLFx_clearcolor_type }

  TXMLFx_clearcolor_type = class(TXMLNode, IXMLFx_clearcolor_type)
  protected
    { IXMLFx_clearcolor_type }
    function Get_Index: LongWord;
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLFx_clearcolor_typeList }

  TXMLFx_clearcolor_typeList = class(TXMLNodeCollection, IXMLFx_clearcolor_typeList)
  protected
    { IXMLFx_clearcolor_typeList }
    function Add: IXMLFx_clearcolor_type;
    function Insert(const Index: Integer): IXMLFx_clearcolor_type;

    function Get_Item(Index: Integer): IXMLFx_clearcolor_type;
  end;

{ TXMLFx_clearstencil_type }

  TXMLFx_clearstencil_type = class(TXMLNode, IXMLFx_clearstencil_type)
  protected
    { IXMLFx_clearstencil_type }
    function Get_Index: LongWord;
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLFx_clearstencil_typeList }

  TXMLFx_clearstencil_typeList = class(TXMLNodeCollection, IXMLFx_clearstencil_typeList)
  protected
    { IXMLFx_clearstencil_typeList }
    function Add: IXMLFx_clearstencil_type;
    function Insert(const Index: Integer): IXMLFx_clearstencil_type;

    function Get_Item(Index: Integer): IXMLFx_clearstencil_type;
  end;

{ TXMLFx_cleardepth_type }

  TXMLFx_cleardepth_type = class(TXMLNode, IXMLFx_cleardepth_type)
  protected
    { IXMLFx_cleardepth_type }
    function Get_Index: LongWord;
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLFx_cleardepth_typeList }

  TXMLFx_cleardepth_typeList = class(TXMLNodeCollection, IXMLFx_cleardepth_typeList)
  protected
    { IXMLFx_cleardepth_typeList }
    function Add: IXMLFx_cleardepth_type;
    function Insert(const Index: Integer): IXMLFx_cleardepth_type;

    function Get_Item(Index: Integer): IXMLFx_cleardepth_type;
  end;

{ TXMLProfile_glsl_type }

  TXMLProfile_glsl_type = class(TXMLNode, IXMLProfile_glsl_type)
  private
    FCode: IXMLFx_code_typeList;
    FInclude: IXMLFx_include_typeList;
    FNewparam: IXMLGlsl_newparam_typeList;
    FTechnique: IXMLProfile_glsl_type_techniqueList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_glsl_type }
    function Get_Id: UnicodeString;
    function Get_Platform: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Code: IXMLFx_code_typeList;
    function Get_Include: IXMLFx_include_typeList;
    function Get_Newparam: IXMLGlsl_newparam_typeList;
    function Get_Technique: IXMLProfile_glsl_type_techniqueList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Platform(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGlsl_newparam_type }

  TXMLGlsl_newparam_type = class(TXMLNode, IXMLGlsl_newparam_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
  protected
    { IXMLGlsl_newparam_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Semantic: UnicodeString;
    function Get_Modifier: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLGlsl_array_type;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
    procedure Set_Modifier(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGlsl_newparam_typeList }

  TXMLGlsl_newparam_typeList = class(TXMLNodeCollection, IXMLGlsl_newparam_typeList)
  protected
    { IXMLGlsl_newparam_typeList }
    function Add: IXMLGlsl_newparam_type;
    function Insert(const Index: Integer): IXMLGlsl_newparam_type;

    function Get_Item(Index: Integer): IXMLGlsl_newparam_type;
  end;

{ TXMLGlsl_array_type }

  TXMLGlsl_array_type = class(TXMLNode, IXMLGlsl_array_type)
  protected
    { IXMLGlsl_array_type }
    function Get_Length: LongWord;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLGlsl_array_type;
    procedure Set_Length(Value: LongWord);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_glsl_type_technique }

  TXMLProfile_glsl_type_technique = class(TXMLNode, IXMLProfile_glsl_type_technique)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FPass: IXMLProfile_glsl_type_technique_passList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_glsl_type_technique }
    function Get_Id: UnicodeString;
    function Get_Sid: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Pass: IXMLProfile_glsl_type_technique_passList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_glsl_type_techniqueList }

  TXMLProfile_glsl_type_techniqueList = class(TXMLNodeCollection, IXMLProfile_glsl_type_techniqueList)
  protected
    { IXMLProfile_glsl_type_techniqueList }
    function Add: IXMLProfile_glsl_type_technique;
    function Insert(const Index: Integer): IXMLProfile_glsl_type_technique;

    function Get_Item(Index: Integer): IXMLProfile_glsl_type_technique;
  end;

{ TXMLProfile_glsl_type_technique_pass }

  TXMLProfile_glsl_type_technique_pass = class(TXMLNode, IXMLProfile_glsl_type_technique_pass)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_glsl_type_technique_pass }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_States: IXMLProfile_glsl_type_technique_pass_states;
    function Get_Program_: IXMLGlsl_program_type;
    function Get_Evaluate: IXMLProfile_glsl_type_technique_pass_evaluate;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_glsl_type_technique_passList }

  TXMLProfile_glsl_type_technique_passList = class(TXMLNodeCollection, IXMLProfile_glsl_type_technique_passList)
  protected
    { IXMLProfile_glsl_type_technique_passList }
    function Add: IXMLProfile_glsl_type_technique_pass;
    function Insert(const Index: Integer): IXMLProfile_glsl_type_technique_pass;

    function Get_Item(Index: Integer): IXMLProfile_glsl_type_technique_pass;
  end;

{ TXMLProfile_glsl_type_technique_pass_states }

  TXMLProfile_glsl_type_technique_pass_states = class(TXMLNode, IXMLProfile_glsl_type_technique_pass_states)
  protected
    { IXMLProfile_glsl_type_technique_pass_states }
    function Get_Alpha_func: IXMLAlpha_func;
    function Get_Blend_func: IXMLBlend_func;
    function Get_Blend_func_separate: IXMLBlend_func_separate;
    function Get_Blend_equation: IXMLBlend_equation;
    function Get_Blend_equation_separate: IXMLBlend_equation_separate;
    function Get_Color_material: IXMLColor_material;
    function Get_Cull_face: IXMLCull_face;
    function Get_Depth_func: IXMLDepth_func;
    function Get_Fog_mode: IXMLFog_mode;
    function Get_Fog_coord_src: IXMLFog_coord_src;
    function Get_Front_face: IXMLFront_face;
    function Get_Light_model_color_control: IXMLLight_model_color_control;
    function Get_Logic_op: IXMLLogic_op;
    function Get_Polygon_mode: IXMLPolygon_mode;
    function Get_Shade_model: IXMLShade_model;
    function Get_Stencil_func: IXMLStencil_func;
    function Get_Stencil_op: IXMLStencil_op;
    function Get_Stencil_func_separate: IXMLStencil_func_separate;
    function Get_Stencil_op_separate: IXMLStencil_op_separate;
    function Get_Stencil_mask_separate: IXMLStencil_mask_separate;
    function Get_Light_enable: IXMLLight_enable;
    function Get_Light_ambient: IXMLLight_ambient;
    function Get_Light_diffuse: IXMLLight_diffuse;
    function Get_Light_specular: IXMLLight_specular;
    function Get_Light_position: IXMLLight_position;
    function Get_Light_constant_attenuation: IXMLLight_constant_attenuation;
    function Get_Light_linear_attenuation: IXMLLight_linear_attenuation;
    function Get_Light_quadratic_attenuation: IXMLLight_quadratic_attenuation;
    function Get_Light_spot_cutoff: IXMLLight_spot_cutoff;
    function Get_Light_spot_direction: IXMLLight_spot_direction;
    function Get_Light_spot_exponent: IXMLLight_spot_exponent;
    function Get_Texture1D: IXMLTexture1D;
    function Get_Texture2D: IXMLTexture2D;
    function Get_Texture3D: IXMLTexture3D;
    function Get_TextureCUBE: IXMLTextureCUBE;
    function Get_TextureRECT: IXMLTextureRECT;
    function Get_TextureDEPTH: IXMLTextureDEPTH;
    function Get_Texture1D_enable: IXMLTexture1D_enable;
    function Get_Texture2D_enable: IXMLTexture2D_enable;
    function Get_Texture3D_enable: IXMLTexture3D_enable;
    function Get_TextureCUBE_enable: IXMLTextureCUBE_enable;
    function Get_TextureRECT_enable: IXMLTextureRECT_enable;
    function Get_TextureDEPTH_enable: IXMLTextureDEPTH_enable;
    function Get_Texture_env_color: IXMLTexture_env_color;
    function Get_Texture_env_mode: IXMLTexture_env_mode;
    function Get_Clip_plane: IXMLClip_plane;
    function Get_Clip_plane_enable: IXMLClip_plane_enable;
    function Get_Blend_color: IXMLBlend_color;
    function Get_Color_mask: IXMLColor_mask;
    function Get_Depth_bounds: IXMLDepth_bounds;
    function Get_Depth_mask: IXMLDepth_mask;
    function Get_Depth_range: IXMLDepth_range;
    function Get_Fog_density: IXMLFog_density;
    function Get_Fog_start: IXMLFog_start;
    function Get_Fog_end: IXMLFog_end;
    function Get_Fog_color: IXMLFog_color;
    function Get_Light_model_ambient: IXMLLight_model_ambient;
    function Get_Lighting_enable: IXMLLighting_enable;
    function Get_Line_stipple: IXMLLine_stipple;
    function Get_Line_width: IXMLLine_width;
    function Get_Material_ambient: IXMLMaterial_ambient;
    function Get_Material_diffuse: IXMLMaterial_diffuse;
    function Get_Material_emission: IXMLMaterial_emission;
    function Get_Material_shininess: IXMLMaterial_shininess;
    function Get_Material_specular: IXMLMaterial_specular;
    function Get_Model_view_matrix: IXMLModel_view_matrix;
    function Get_Point_distance_attenuation: IXMLPoint_distance_attenuation;
    function Get_Point_fade_threshold_size: IXMLPoint_fade_threshold_size;
    function Get_Point_size: IXMLPoint_size;
    function Get_Point_size_min: IXMLPoint_size_min;
    function Get_Point_size_max: IXMLPoint_size_max;
    function Get_Polygon_offset: IXMLPolygon_offset;
    function Get_Projection_matrix: IXMLProjection_matrix;
    function Get_Scissor: IXMLScissor;
    function Get_Stencil_mask: IXMLStencil_mask;
    function Get_Alpha_test_enable: IXMLAlpha_test_enable;
    function Get_Blend_enable: IXMLBlend_enable;
    function Get_Color_logic_op_enable: IXMLColor_logic_op_enable;
    function Get_Color_material_enable: IXMLColor_material_enable;
    function Get_Cull_face_enable: IXMLCull_face_enable;
    function Get_Depth_bounds_enable: IXMLDepth_bounds_enable;
    function Get_Depth_clamp_enable: IXMLDepth_clamp_enable;
    function Get_Depth_test_enable: IXMLDepth_test_enable;
    function Get_Dither_enable: IXMLDither_enable;
    function Get_Fog_enable: IXMLFog_enable;
    function Get_Light_model_local_viewer_enable: IXMLLight_model_local_viewer_enable;
    function Get_Light_model_two_side_enable: IXMLLight_model_two_side_enable;
    function Get_Line_smooth_enable: IXMLLine_smooth_enable;
    function Get_Line_stipple_enable: IXMLLine_stipple_enable;
    function Get_Logic_op_enable: IXMLLogic_op_enable;
    function Get_Multisample_enable: IXMLMultisample_enable;
    function Get_Normalize_enable: IXMLNormalize_enable;
    function Get_Point_smooth_enable: IXMLPoint_smooth_enable;
    function Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
    function Get_Polygon_offset_line_enable: IXMLPolygon_offset_line_enable;
    function Get_Polygon_offset_point_enable: IXMLPolygon_offset_point_enable;
    function Get_Polygon_smooth_enable: IXMLPolygon_smooth_enable;
    function Get_Polygon_stipple_enable: IXMLPolygon_stipple_enable;
    function Get_Rescale_normal_enable: IXMLRescale_normal_enable;
    function Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
    function Get_Sample_alpha_to_one_enable: IXMLSample_alpha_to_one_enable;
    function Get_Sample_coverage_enable: IXMLSample_coverage_enable;
    function Get_Scissor_test_enable: IXMLScissor_test_enable;
    function Get_Stencil_test_enable: IXMLStencil_test_enable;
  public
    procedure AfterConstruction; override;
  end;



  { TXMLString_List }

  TXMLString_List = class(TXMLNodeCollection, IXMLString_List)
  protected
    { IXMLString_List }
    function Add(const Value: UnicodeString): IXMLNode;
    function Insert(const Index: Integer; const Value: UnicodeString): IXMLNode;

    function Get_Item(Index: Integer): UnicodeString;
  end;

  { TXMLGlsl_program_type }

  TXMLGlsl_program_type = class(TXMLNode, IXMLGlsl_program_type)
  private
    FShader: IXMLGlsl_shader_typeList;
    FBind_attribute: IXMLGlsl_program_type_bind_attributeList;
    FBind_uniform: IXMLGlsl_program_type_bind_uniformList;
  protected
    { IXMLGlsl_program_type }
    function Get_Shader: IXMLGlsl_shader_typeList;
    function Get_Bind_attribute: IXMLGlsl_program_type_bind_attributeList;
    function Get_Bind_uniform: IXMLGlsl_program_type_bind_uniformList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGlsl_shader_type }

  TXMLGlsl_shader_type = class(TXMLNode, IXMLGlsl_shader_type)
  private
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLGlsl_shader_type }
    function Get_Stage: UnicodeString;
    function Get_Sources: IXMLFx_sources_type;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Stage(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGlsl_shader_typeList }

  TXMLGlsl_shader_typeList = class(TXMLNodeCollection, IXMLGlsl_shader_typeList)
  protected
    { IXMLGlsl_shader_typeList }
    function Add: IXMLGlsl_shader_type;
    function Insert(const Index: Integer): IXMLGlsl_shader_type;

    function Get_Item(Index: Integer): IXMLGlsl_shader_type;
  end;

{ TXMLGlsl_program_type_bind_attribute }

  TXMLGlsl_program_type_bind_attribute = class(TXMLNode, IXMLGlsl_program_type_bind_attribute)
  protected
    { IXMLGlsl_program_type_bind_attribute }
    function Get_Symbol: UnicodeString;
    function Get_Semantic: UnicodeString;
    procedure Set_Symbol(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
  end;

{ TXMLGlsl_program_type_bind_attributeList }

  TXMLGlsl_program_type_bind_attributeList = class(TXMLNodeCollection, IXMLGlsl_program_type_bind_attributeList)
  protected
    { IXMLGlsl_program_type_bind_attributeList }
    function Add: IXMLGlsl_program_type_bind_attribute;
    function Insert(const Index: Integer): IXMLGlsl_program_type_bind_attribute;

    function Get_Item(Index: Integer): IXMLGlsl_program_type_bind_attribute;
  end;

{ TXMLGlsl_program_type_bind_uniform }

  TXMLGlsl_program_type_bind_uniform = class(TXMLNode, IXMLGlsl_program_type_bind_uniform)
  protected
    { IXMLGlsl_program_type_bind_uniform }
    function Get_Symbol: UnicodeString;
    function Get_Param: IXMLGlsl_program_type_bind_uniform_param;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLGlsl_array_type;
    procedure Set_Symbol(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGlsl_program_type_bind_uniformList }

  TXMLGlsl_program_type_bind_uniformList = class(TXMLNodeCollection, IXMLGlsl_program_type_bind_uniformList)
  protected
    { IXMLGlsl_program_type_bind_uniformList }
    function Add: IXMLGlsl_program_type_bind_uniform;
    function Insert(const Index: Integer): IXMLGlsl_program_type_bind_uniform;

    function Get_Item(Index: Integer): IXMLGlsl_program_type_bind_uniform;
  end;

{ TXMLGlsl_program_type_bind_uniform_param }

  TXMLGlsl_program_type_bind_uniform_param = class(TXMLNode, IXMLGlsl_program_type_bind_uniform_param)
  protected
    { IXMLGlsl_program_type_bind_uniform_param }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLProfile_glsl_type_technique_pass_evaluate }

  TXMLProfile_glsl_type_technique_pass_evaluate = class(TXMLNode, IXMLProfile_glsl_type_technique_pass_evaluate)
  private
    FColor_target: IXMLFx_colortarget_typeList;
    FDepth_target: IXMLFx_depthtarget_typeList;
    FStencil_target: IXMLFx_stenciltarget_typeList;
    FColor_clear: IXMLFx_clearcolor_typeList;
    FDepth_clear: IXMLFx_cleardepth_typeList;
    FStencil_clear: IXMLFx_clearstencil_typeList;
  protected
    { IXMLProfile_glsl_type_technique_pass_evaluate }
    function Get_Color_target: IXMLFx_colortarget_typeList;
    function Get_Depth_target: IXMLFx_depthtarget_typeList;
    function Get_Stencil_target: IXMLFx_stenciltarget_typeList;
    function Get_Color_clear: IXMLFx_clearcolor_typeList;
    function Get_Depth_clear: IXMLFx_cleardepth_typeList;
    function Get_Stencil_clear: IXMLFx_clearstencil_typeList;
    function Get_Draw: UnicodeString;
    procedure Set_Draw(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

  { TXMLAlpha_func }

  TXMLAlpha_func = class(TXMLNode, IXMLAlpha_func)
  protected
    { IXMLAlpha_func }
    function Get_Func: IXMLAlpha_func_func;
    function Get_Value: IXMLAlpha_func_value;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAlpha_func_func }

  TXMLAlpha_func_func = class(TXMLNode, IXMLAlpha_func_func)
  protected
    { IXMLAlpha_func_func }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLAlpha_func_value }

  TXMLAlpha_func_value = class(TXMLNode, IXMLAlpha_func_value)
  protected
    { IXMLAlpha_func_value }
    function Get_Value: Single;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Single);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLColor_material }

  TXMLColor_material = class(TXMLNode, IXMLColor_material)
  protected
    { IXMLColor_material }
    function Get_Face: IXMLColor_material_face;
    function Get_Mode: IXMLColor_material_mode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLColor_material_face }

  TXMLColor_material_face = class(TXMLNode, IXMLColor_material_face)
  protected
    { IXMLColor_material_face }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLColor_material_mode }

  TXMLColor_material_mode = class(TXMLNode, IXMLColor_material_mode)
  protected
    { IXMLColor_material_mode }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_mode }

  TXMLFog_mode = class(TXMLNode, IXMLFog_mode)
  protected
    { IXMLFog_mode }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_coord_src }

  TXMLFog_coord_src = class(TXMLNode, IXMLFog_coord_src)
  protected
    { IXMLFog_coord_src }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLight_model_color_control }

  TXMLLight_model_color_control = class(TXMLNode, IXMLLight_model_color_control)
  protected
    { IXMLLight_model_color_control }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLogic_op }

  TXMLLogic_op = class(TXMLNode, IXMLLogic_op)
  protected
    { IXMLLogic_op }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_mode }

  TXMLPolygon_mode = class(TXMLNode, IXMLPolygon_mode)
  protected
    { IXMLPolygon_mode }
    function Get_Face: IXMLPolygon_mode_face;
    function Get_Mode: IXMLPolygon_mode_mode;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPolygon_mode_face }

  TXMLPolygon_mode_face = class(TXMLNode, IXMLPolygon_mode_face)
  protected
    { IXMLPolygon_mode_face }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_mode_mode }

  TXMLPolygon_mode_mode = class(TXMLNode, IXMLPolygon_mode_mode)
  protected
    { IXMLPolygon_mode_mode }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLShade_model }

  TXMLShade_model = class(TXMLNode, IXMLShade_model)
  protected
    { IXMLShade_model }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLight_enable }

  TXMLLight_enable = class(TXMLNode, IXMLLight_enable)
  protected
    { IXMLLight_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_ambient }

  TXMLLight_ambient = class(TXMLNode, IXMLLight_ambient)
  protected
    { IXMLLight_ambient }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_diffuse }

  TXMLLight_diffuse = class(TXMLNode, IXMLLight_diffuse)
  protected
    { IXMLLight_diffuse }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_specular }

  TXMLLight_specular = class(TXMLNode, IXMLLight_specular)
  protected
    { IXMLLight_specular }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_position }

  TXMLLight_position = class(TXMLNode, IXMLLight_position)
  protected
    { IXMLLight_position }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_constant_attenuation }

  TXMLLight_constant_attenuation = class(TXMLNode, IXMLLight_constant_attenuation)
  protected
    { IXMLLight_constant_attenuation }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_linear_attenuation }

  TXMLLight_linear_attenuation = class(TXMLNode, IXMLLight_linear_attenuation)
  protected
    { IXMLLight_linear_attenuation }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_quadratic_attenuation }

  TXMLLight_quadratic_attenuation = class(TXMLNode, IXMLLight_quadratic_attenuation)
  protected
    { IXMLLight_quadratic_attenuation }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_spot_cutoff }

  TXMLLight_spot_cutoff = class(TXMLNode, IXMLLight_spot_cutoff)
  protected
    { IXMLLight_spot_cutoff }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_spot_direction }

  TXMLLight_spot_direction = class(TXMLNode, IXMLLight_spot_direction)
  protected
    { IXMLLight_spot_direction }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLLight_spot_exponent }

  TXMLLight_spot_exponent = class(TXMLNode, IXMLLight_spot_exponent)
  protected
    { IXMLLight_spot_exponent }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTexture1D }

  TXMLTexture1D = class(TXMLNode, IXMLTexture1D)
  protected
    { IXMLTexture1D }
    function Get_Index: LongWord;
    function Get_Value: IXMLFx_sampler1D_type;
    function Get_Param: UnicodeString;
    procedure Set_Index(Value: LongWord);
    procedure Set_Param(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTexture2D }

  TXMLTexture2D = class(TXMLNode, IXMLTexture2D)
  protected
    { IXMLTexture2D }
    function Get_Index: LongWord;
    function Get_Value: IXMLFx_sampler2D_type;
    function Get_Param: UnicodeString;
    procedure Set_Index(Value: LongWord);
    procedure Set_Param(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTexture3D }

  TXMLTexture3D = class(TXMLNode, IXMLTexture3D)
  protected
    { IXMLTexture3D }
    function Get_Index: LongWord;
    function Get_Value: IXMLFx_sampler3D_type;
    function Get_Param: UnicodeString;
    procedure Set_Index(Value: LongWord);
    procedure Set_Param(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextureCUBE }

  TXMLTextureCUBE = class(TXMLNode, IXMLTextureCUBE)
  protected
    { IXMLTextureCUBE }
    function Get_Index: LongWord;
    function Get_Value: IXMLFx_samplerCUBE_type;
    function Get_Param: UnicodeString;
    procedure Set_Index(Value: LongWord);
    procedure Set_Param(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextureRECT }

  TXMLTextureRECT = class(TXMLNode, IXMLTextureRECT)
  protected
    { IXMLTextureRECT }
    function Get_Index: LongWord;
    function Get_Value: IXMLFx_samplerRECT_type;
    function Get_Param: UnicodeString;
    procedure Set_Index(Value: LongWord);
    procedure Set_Param(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTextureDEPTH }

  TXMLTextureDEPTH = class(TXMLNode, IXMLTextureDEPTH)
  protected
    { IXMLTextureDEPTH }
    function Get_Index: LongWord;
    function Get_Value: IXMLFx_samplerDEPTH_type;
    function Get_Param: UnicodeString;
    procedure Set_Index(Value: LongWord);
    procedure Set_Param(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTexture1D_enable }

  TXMLTexture1D_enable = class(TXMLNode, IXMLTexture1D_enable)
  protected
    { IXMLTexture1D_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTexture2D_enable }

  TXMLTexture2D_enable = class(TXMLNode, IXMLTexture2D_enable)
  protected
    { IXMLTexture2D_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTexture3D_enable }

  TXMLTexture3D_enable = class(TXMLNode, IXMLTexture3D_enable)
  protected
    { IXMLTexture3D_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTextureCUBE_enable }

  TXMLTextureCUBE_enable = class(TXMLNode, IXMLTextureCUBE_enable)
  protected
    { IXMLTextureCUBE_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTextureRECT_enable }

  TXMLTextureRECT_enable = class(TXMLNode, IXMLTextureRECT_enable)
  protected
    { IXMLTextureRECT_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTextureDEPTH_enable }

  TXMLTextureDEPTH_enable = class(TXMLNode, IXMLTextureDEPTH_enable)
  protected
    { IXMLTextureDEPTH_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTexture_env_color }

  TXMLTexture_env_color = class(TXMLNode, IXMLTexture_env_color)
  protected
    { IXMLTexture_env_color }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLTexture_env_mode }

  TXMLTexture_env_mode = class(TXMLNode, IXMLTexture_env_mode)
  protected
    { IXMLTexture_env_mode }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLClip_plane }

  TXMLClip_plane = class(TXMLNode, IXMLClip_plane)
  protected
    { IXMLClip_plane }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLClip_plane_enable }

  TXMLClip_plane_enable = class(TXMLNode, IXMLClip_plane_enable)
  protected
    { IXMLClip_plane_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    function Get_Index: LongWord;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
    procedure Set_Index(Value: LongWord);
  end;

{ TXMLDepth_bounds }

  TXMLDepth_bounds = class(TXMLNode, IXMLDepth_bounds)
  protected
    { IXMLDepth_bounds }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_density }

  TXMLFog_density = class(TXMLNode, IXMLFog_density)
  protected
    { IXMLFog_density }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_start }

  TXMLFog_start = class(TXMLNode, IXMLFog_start)
  protected
    { IXMLFog_start }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_end }

  TXMLFog_end = class(TXMLNode, IXMLFog_end)
  protected
    { IXMLFog_end }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_color }

  TXMLFog_color = class(TXMLNode, IXMLFog_color)
  protected
    { IXMLFog_color }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLight_model_ambient }

  TXMLLight_model_ambient = class(TXMLNode, IXMLLight_model_ambient)
  protected
    { IXMLLight_model_ambient }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLighting_enable }

  TXMLLighting_enable = class(TXMLNode, IXMLLighting_enable)
  protected
    { IXMLLighting_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLine_stipple }

  TXMLLine_stipple = class(TXMLNode, IXMLLine_stipple)
  protected
    { IXMLLine_stipple }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLMaterial_ambient }

  TXMLMaterial_ambient = class(TXMLNode, IXMLMaterial_ambient)
  protected
    { IXMLMaterial_ambient }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLMaterial_diffuse }

  TXMLMaterial_diffuse = class(TXMLNode, IXMLMaterial_diffuse)
  protected
    { IXMLMaterial_diffuse }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLMaterial_emission }

  TXMLMaterial_emission = class(TXMLNode, IXMLMaterial_emission)
  protected
    { IXMLMaterial_emission }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLMaterial_shininess }

  TXMLMaterial_shininess = class(TXMLNode, IXMLMaterial_shininess)
  protected
    { IXMLMaterial_shininess }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLMaterial_specular }

  TXMLMaterial_specular = class(TXMLNode, IXMLMaterial_specular)
  protected
    { IXMLMaterial_specular }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLModel_view_matrix }

  TXMLModel_view_matrix = class(TXMLNode, IXMLModel_view_matrix)
  protected
    { IXMLModel_view_matrix }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_distance_attenuation }

  TXMLPoint_distance_attenuation = class(TXMLNode, IXMLPoint_distance_attenuation)
  protected
    { IXMLPoint_distance_attenuation }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_fade_threshold_size }

  TXMLPoint_fade_threshold_size = class(TXMLNode, IXMLPoint_fade_threshold_size)
  protected
    { IXMLPoint_fade_threshold_size }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_size_min }

  TXMLPoint_size_min = class(TXMLNode, IXMLPoint_size_min)
  protected
    { IXMLPoint_size_min }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_size_max }

  TXMLPoint_size_max = class(TXMLNode, IXMLPoint_size_max)
  protected
    { IXMLPoint_size_max }
    function Get_Value: Double;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Double);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLProjection_matrix }

  TXMLProjection_matrix = class(TXMLNode, IXMLProjection_matrix)
  protected
    { IXMLProjection_matrix }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLAlpha_test_enable }

  TXMLAlpha_test_enable = class(TXMLNode, IXMLAlpha_test_enable)
  protected
    { IXMLAlpha_test_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLColor_logic_op_enable }

  TXMLColor_logic_op_enable = class(TXMLNode, IXMLColor_logic_op_enable)
  protected
    { IXMLColor_logic_op_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLColor_material_enable }

  TXMLColor_material_enable = class(TXMLNode, IXMLColor_material_enable)
  protected
    { IXMLColor_material_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDepth_bounds_enable }

  TXMLDepth_bounds_enable = class(TXMLNode, IXMLDepth_bounds_enable)
  protected
    { IXMLDepth_bounds_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLDepth_clamp_enable }

  TXMLDepth_clamp_enable = class(TXMLNode, IXMLDepth_clamp_enable)
  protected
    { IXMLDepth_clamp_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLFog_enable }

  TXMLFog_enable = class(TXMLNode, IXMLFog_enable)
  protected
    { IXMLFog_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLight_model_local_viewer_enable }

  TXMLLight_model_local_viewer_enable = class(TXMLNode, IXMLLight_model_local_viewer_enable)
  protected
    { IXMLLight_model_local_viewer_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLight_model_two_side_enable }

  TXMLLight_model_two_side_enable = class(TXMLNode, IXMLLight_model_two_side_enable)
  protected
    { IXMLLight_model_two_side_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLine_smooth_enable }

  TXMLLine_smooth_enable = class(TXMLNode, IXMLLine_smooth_enable)
  protected
    { IXMLLine_smooth_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLine_stipple_enable }

  TXMLLine_stipple_enable = class(TXMLNode, IXMLLine_stipple_enable)
  protected
    { IXMLLine_stipple_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLLogic_op_enable }

  TXMLLogic_op_enable = class(TXMLNode, IXMLLogic_op_enable)
  protected
    { IXMLLogic_op_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLMultisample_enable }

  TXMLMultisample_enable = class(TXMLNode, IXMLMultisample_enable)
  protected
    { IXMLMultisample_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLNormalize_enable }

  TXMLNormalize_enable = class(TXMLNode, IXMLNormalize_enable)
  protected
    { IXMLNormalize_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPoint_smooth_enable }

  TXMLPoint_smooth_enable = class(TXMLNode, IXMLPoint_smooth_enable)
  protected
    { IXMLPoint_smooth_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_offset_line_enable }

  TXMLPolygon_offset_line_enable = class(TXMLNode, IXMLPolygon_offset_line_enable)
  protected
    { IXMLPolygon_offset_line_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_offset_point_enable }

  TXMLPolygon_offset_point_enable = class(TXMLNode, IXMLPolygon_offset_point_enable)
  protected
    { IXMLPolygon_offset_point_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_smooth_enable }

  TXMLPolygon_smooth_enable = class(TXMLNode, IXMLPolygon_smooth_enable)
  protected
    { IXMLPolygon_smooth_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLPolygon_stipple_enable }

  TXMLPolygon_stipple_enable = class(TXMLNode, IXMLPolygon_stipple_enable)
  protected
    { IXMLPolygon_stipple_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLRescale_normal_enable }

  TXMLRescale_normal_enable = class(TXMLNode, IXMLRescale_normal_enable)
  protected
    { IXMLRescale_normal_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLSample_alpha_to_one_enable }

  TXMLSample_alpha_to_one_enable = class(TXMLNode, IXMLSample_alpha_to_one_enable)
  protected
    { IXMLSample_alpha_to_one_enable }
    function Get_Value: Boolean;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: Boolean);
    procedure Set_Param(Value: UnicodeString);
  end;


  { TXMLFx_common_newparam_type }

  TXMLFx_common_newparam_type = class(TXMLNode, IXMLFx_common_newparam_type)
  protected
    { IXMLFx_common_newparam_type }
    function Get_Sid: UnicodeString;
    function Get_Semantic: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_common_newparam_typeList }

  TXMLFx_common_newparam_typeList = class(TXMLNodeCollection, IXMLFx_common_newparam_typeList)
  protected
    { IXMLFx_common_newparam_typeList }
    function Add: IXMLFx_common_newparam_type;
    function Insert(const Index: Integer): IXMLFx_common_newparam_type;

    function Get_Item(Index: Integer): IXMLFx_common_newparam_type;
  end;

  { TXMLProfile_cg_type }

  TXMLProfile_cg_type = class(TXMLNode, IXMLProfile_cg_type)
  private
    FCode: IXMLFx_code_typeList;
    FInclude: IXMLFx_include_typeList;
    FNewparam: IXMLCg_newparam_typeList;
    FTechnique: IXMLProfile_cg_type_techniqueList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_cg_type }
    function Get_Id: UnicodeString;
    function Get_Platform: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Code: IXMLFx_code_typeList;
    function Get_Include: IXMLFx_include_typeList;
    function Get_Newparam: IXMLCg_newparam_typeList;
    function Get_Technique: IXMLProfile_cg_type_techniqueList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Platform(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_newparam_type }

  TXMLCg_newparam_type = class(TXMLNode, IXMLCg_newparam_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
  protected
    { IXMLCg_newparam_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Semantic: UnicodeString;
    function Get_Modifier: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Bool2x1: UnicodeString;
    function Get_Bool2x2: UnicodeString;
    function Get_Bool2x3: UnicodeString;
    function Get_Bool2x4: UnicodeString;
    function Get_Bool3x1: UnicodeString;
    function Get_Bool3x2: UnicodeString;
    function Get_Bool3x3: UnicodeString;
    function Get_Bool3x4: UnicodeString;
    function Get_Bool4x1: UnicodeString;
    function Get_Bool4x2: UnicodeString;
    function Get_Bool4x3: UnicodeString;
    function Get_Bool4x4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x1: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float2x3: UnicodeString;
    function Get_Float2x4: UnicodeString;
    function Get_Float3x1: UnicodeString;
    function Get_Float3x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float3x4: UnicodeString;
    function Get_Float4x1: UnicodeString;
    function Get_Float4x2: UnicodeString;
    function Get_Float4x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Int2x1: UnicodeString;
    function Get_Int2x2: UnicodeString;
    function Get_Int2x3: UnicodeString;
    function Get_Int2x4: UnicodeString;
    function Get_Int3x1: UnicodeString;
    function Get_Int3x2: UnicodeString;
    function Get_Int3x3: UnicodeString;
    function Get_Int3x4: UnicodeString;
    function Get_Int4x1: UnicodeString;
    function Get_Int4x2: UnicodeString;
    function Get_Int4x3: UnicodeString;
    function Get_Int4x4: UnicodeString;
    function Get_Half: Double;
    function Get_Half2: UnicodeString;
    function Get_Half3: UnicodeString;
    function Get_Half4: UnicodeString;
    function Get_Half2x1: UnicodeString;
    function Get_Half2x2: UnicodeString;
    function Get_Half2x3: UnicodeString;
    function Get_Half2x4: UnicodeString;
    function Get_Half3x1: UnicodeString;
    function Get_Half3x2: UnicodeString;
    function Get_Half3x3: UnicodeString;
    function Get_Half3x4: UnicodeString;
    function Get_Half4x1: UnicodeString;
    function Get_Half4x2: UnicodeString;
    function Get_Half4x3: UnicodeString;
    function Get_Half4x4: UnicodeString;
    function Get_Fixed: Double;
    function Get_Fixed2: UnicodeString;
    function Get_Fixed3: UnicodeString;
    function Get_Fixed4: UnicodeString;
    function Get_Fixed2x1: UnicodeString;
    function Get_Fixed2x2: UnicodeString;
    function Get_Fixed2x3: UnicodeString;
    function Get_Fixed2x4: UnicodeString;
    function Get_Fixed3x1: UnicodeString;
    function Get_Fixed3x2: UnicodeString;
    function Get_Fixed3x3: UnicodeString;
    function Get_Fixed3x4: UnicodeString;
    function Get_Fixed4x1: UnicodeString;
    function Get_Fixed4x2: UnicodeString;
    function Get_Fixed4x3: UnicodeString;
    function Get_Fixed4x4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_String_: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLCg_array_type;
    function Get_Usertype: IXMLCg_user_type;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
    procedure Set_Modifier(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Bool2x1(Value: UnicodeString);
    procedure Set_Bool2x2(Value: UnicodeString);
    procedure Set_Bool2x3(Value: UnicodeString);
    procedure Set_Bool2x4(Value: UnicodeString);
    procedure Set_Bool3x1(Value: UnicodeString);
    procedure Set_Bool3x2(Value: UnicodeString);
    procedure Set_Bool3x3(Value: UnicodeString);
    procedure Set_Bool3x4(Value: UnicodeString);
    procedure Set_Bool4x1(Value: UnicodeString);
    procedure Set_Bool4x2(Value: UnicodeString);
    procedure Set_Bool4x3(Value: UnicodeString);
    procedure Set_Bool4x4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x1(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float2x3(Value: UnicodeString);
    procedure Set_Float2x4(Value: UnicodeString);
    procedure Set_Float3x1(Value: UnicodeString);
    procedure Set_Float3x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float3x4(Value: UnicodeString);
    procedure Set_Float4x1(Value: UnicodeString);
    procedure Set_Float4x2(Value: UnicodeString);
    procedure Set_Float4x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Int2x1(Value: UnicodeString);
    procedure Set_Int2x2(Value: UnicodeString);
    procedure Set_Int2x3(Value: UnicodeString);
    procedure Set_Int2x4(Value: UnicodeString);
    procedure Set_Int3x1(Value: UnicodeString);
    procedure Set_Int3x2(Value: UnicodeString);
    procedure Set_Int3x3(Value: UnicodeString);
    procedure Set_Int3x4(Value: UnicodeString);
    procedure Set_Int4x1(Value: UnicodeString);
    procedure Set_Int4x2(Value: UnicodeString);
    procedure Set_Int4x3(Value: UnicodeString);
    procedure Set_Int4x4(Value: UnicodeString);
    procedure Set_Half(Value: Double);
    procedure Set_Half2(Value: UnicodeString);
    procedure Set_Half3(Value: UnicodeString);
    procedure Set_Half4(Value: UnicodeString);
    procedure Set_Half2x1(Value: UnicodeString);
    procedure Set_Half2x2(Value: UnicodeString);
    procedure Set_Half2x3(Value: UnicodeString);
    procedure Set_Half2x4(Value: UnicodeString);
    procedure Set_Half3x1(Value: UnicodeString);
    procedure Set_Half3x2(Value: UnicodeString);
    procedure Set_Half3x3(Value: UnicodeString);
    procedure Set_Half3x4(Value: UnicodeString);
    procedure Set_Half4x1(Value: UnicodeString);
    procedure Set_Half4x2(Value: UnicodeString);
    procedure Set_Half4x3(Value: UnicodeString);
    procedure Set_Half4x4(Value: UnicodeString);
    procedure Set_Fixed(Value: Double);
    procedure Set_Fixed2(Value: UnicodeString);
    procedure Set_Fixed3(Value: UnicodeString);
    procedure Set_Fixed4(Value: UnicodeString);
    procedure Set_Fixed2x1(Value: UnicodeString);
    procedure Set_Fixed2x2(Value: UnicodeString);
    procedure Set_Fixed2x3(Value: UnicodeString);
    procedure Set_Fixed2x4(Value: UnicodeString);
    procedure Set_Fixed3x1(Value: UnicodeString);
    procedure Set_Fixed3x2(Value: UnicodeString);
    procedure Set_Fixed3x3(Value: UnicodeString);
    procedure Set_Fixed3x4(Value: UnicodeString);
    procedure Set_Fixed4x1(Value: UnicodeString);
    procedure Set_Fixed4x2(Value: UnicodeString);
    procedure Set_Fixed4x3(Value: UnicodeString);
    procedure Set_Fixed4x4(Value: UnicodeString);
    procedure Set_String_(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_newparam_typeList }

  TXMLCg_newparam_typeList = class(TXMLNodeCollection, IXMLCg_newparam_typeList)
  protected
    { IXMLCg_newparam_typeList }
    function Add: IXMLCg_newparam_type;
    function Insert(const Index: Integer): IXMLCg_newparam_type;

    function Get_Item(Index: Integer): IXMLCg_newparam_type;
  end;

{ TXMLCg_array_type }

  TXMLCg_array_type = class(TXMLNode, IXMLCg_array_type)
  protected
    { IXMLCg_array_type }
    function Get_Length: LongWord;
    function Get_Resizable: Boolean;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Bool2x1: UnicodeString;
    function Get_Bool2x2: UnicodeString;
    function Get_Bool2x3: UnicodeString;
    function Get_Bool2x4: UnicodeString;
    function Get_Bool3x1: UnicodeString;
    function Get_Bool3x2: UnicodeString;
    function Get_Bool3x3: UnicodeString;
    function Get_Bool3x4: UnicodeString;
    function Get_Bool4x1: UnicodeString;
    function Get_Bool4x2: UnicodeString;
    function Get_Bool4x3: UnicodeString;
    function Get_Bool4x4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x1: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float2x3: UnicodeString;
    function Get_Float2x4: UnicodeString;
    function Get_Float3x1: UnicodeString;
    function Get_Float3x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float3x4: UnicodeString;
    function Get_Float4x1: UnicodeString;
    function Get_Float4x2: UnicodeString;
    function Get_Float4x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Int2x1: UnicodeString;
    function Get_Int2x2: UnicodeString;
    function Get_Int2x3: UnicodeString;
    function Get_Int2x4: UnicodeString;
    function Get_Int3x1: UnicodeString;
    function Get_Int3x2: UnicodeString;
    function Get_Int3x3: UnicodeString;
    function Get_Int3x4: UnicodeString;
    function Get_Int4x1: UnicodeString;
    function Get_Int4x2: UnicodeString;
    function Get_Int4x3: UnicodeString;
    function Get_Int4x4: UnicodeString;
    function Get_Half: Double;
    function Get_Half2: UnicodeString;
    function Get_Half3: UnicodeString;
    function Get_Half4: UnicodeString;
    function Get_Half2x1: UnicodeString;
    function Get_Half2x2: UnicodeString;
    function Get_Half2x3: UnicodeString;
    function Get_Half2x4: UnicodeString;
    function Get_Half3x1: UnicodeString;
    function Get_Half3x2: UnicodeString;
    function Get_Half3x3: UnicodeString;
    function Get_Half3x4: UnicodeString;
    function Get_Half4x1: UnicodeString;
    function Get_Half4x2: UnicodeString;
    function Get_Half4x3: UnicodeString;
    function Get_Half4x4: UnicodeString;
    function Get_Fixed: Double;
    function Get_Fixed2: UnicodeString;
    function Get_Fixed3: UnicodeString;
    function Get_Fixed4: UnicodeString;
    function Get_Fixed2x1: UnicodeString;
    function Get_Fixed2x2: UnicodeString;
    function Get_Fixed2x3: UnicodeString;
    function Get_Fixed2x4: UnicodeString;
    function Get_Fixed3x1: UnicodeString;
    function Get_Fixed3x2: UnicodeString;
    function Get_Fixed3x3: UnicodeString;
    function Get_Fixed3x4: UnicodeString;
    function Get_Fixed4x1: UnicodeString;
    function Get_Fixed4x2: UnicodeString;
    function Get_Fixed4x3: UnicodeString;
    function Get_Fixed4x4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_String_: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLCg_array_type;
    function Get_Usertype: IXMLCg_user_type;
    procedure Set_Length(Value: LongWord);
    procedure Set_Resizable(Value: Boolean);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Bool2x1(Value: UnicodeString);
    procedure Set_Bool2x2(Value: UnicodeString);
    procedure Set_Bool2x3(Value: UnicodeString);
    procedure Set_Bool2x4(Value: UnicodeString);
    procedure Set_Bool3x1(Value: UnicodeString);
    procedure Set_Bool3x2(Value: UnicodeString);
    procedure Set_Bool3x3(Value: UnicodeString);
    procedure Set_Bool3x4(Value: UnicodeString);
    procedure Set_Bool4x1(Value: UnicodeString);
    procedure Set_Bool4x2(Value: UnicodeString);
    procedure Set_Bool4x3(Value: UnicodeString);
    procedure Set_Bool4x4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x1(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float2x3(Value: UnicodeString);
    procedure Set_Float2x4(Value: UnicodeString);
    procedure Set_Float3x1(Value: UnicodeString);
    procedure Set_Float3x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float3x4(Value: UnicodeString);
    procedure Set_Float4x1(Value: UnicodeString);
    procedure Set_Float4x2(Value: UnicodeString);
    procedure Set_Float4x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Int2x1(Value: UnicodeString);
    procedure Set_Int2x2(Value: UnicodeString);
    procedure Set_Int2x3(Value: UnicodeString);
    procedure Set_Int2x4(Value: UnicodeString);
    procedure Set_Int3x1(Value: UnicodeString);
    procedure Set_Int3x2(Value: UnicodeString);
    procedure Set_Int3x3(Value: UnicodeString);
    procedure Set_Int3x4(Value: UnicodeString);
    procedure Set_Int4x1(Value: UnicodeString);
    procedure Set_Int4x2(Value: UnicodeString);
    procedure Set_Int4x3(Value: UnicodeString);
    procedure Set_Int4x4(Value: UnicodeString);
    procedure Set_Half(Value: Double);
    procedure Set_Half2(Value: UnicodeString);
    procedure Set_Half3(Value: UnicodeString);
    procedure Set_Half4(Value: UnicodeString);
    procedure Set_Half2x1(Value: UnicodeString);
    procedure Set_Half2x2(Value: UnicodeString);
    procedure Set_Half2x3(Value: UnicodeString);
    procedure Set_Half2x4(Value: UnicodeString);
    procedure Set_Half3x1(Value: UnicodeString);
    procedure Set_Half3x2(Value: UnicodeString);
    procedure Set_Half3x3(Value: UnicodeString);
    procedure Set_Half3x4(Value: UnicodeString);
    procedure Set_Half4x1(Value: UnicodeString);
    procedure Set_Half4x2(Value: UnicodeString);
    procedure Set_Half4x3(Value: UnicodeString);
    procedure Set_Half4x4(Value: UnicodeString);
    procedure Set_Fixed(Value: Double);
    procedure Set_Fixed2(Value: UnicodeString);
    procedure Set_Fixed3(Value: UnicodeString);
    procedure Set_Fixed4(Value: UnicodeString);
    procedure Set_Fixed2x1(Value: UnicodeString);
    procedure Set_Fixed2x2(Value: UnicodeString);
    procedure Set_Fixed2x3(Value: UnicodeString);
    procedure Set_Fixed2x4(Value: UnicodeString);
    procedure Set_Fixed3x1(Value: UnicodeString);
    procedure Set_Fixed3x2(Value: UnicodeString);
    procedure Set_Fixed3x3(Value: UnicodeString);
    procedure Set_Fixed3x4(Value: UnicodeString);
    procedure Set_Fixed4x1(Value: UnicodeString);
    procedure Set_Fixed4x2(Value: UnicodeString);
    procedure Set_Fixed4x3(Value: UnicodeString);
    procedure Set_Fixed4x4(Value: UnicodeString);
    procedure Set_String_(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_user_type }

  TXMLCg_user_type = class(TXMLNodeCollection, IXMLCg_user_type)
  protected
    { IXMLCg_user_type }
    function Get_Typename: UnicodeString;
    function Get_Source: UnicodeString;
    function Get_Setparam(Index: Integer): IXMLCg_setparam_type;
    procedure Set_Typename(Value: UnicodeString);
    procedure Set_Source(Value: UnicodeString);
    function Add: IXMLCg_setparam_type;
    function Insert(const Index: Integer): IXMLCg_setparam_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_setparam_type }

  TXMLCg_setparam_type = class(TXMLNode, IXMLCg_setparam_type)
  protected
    { IXMLCg_setparam_type }
    function Get_Ref: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Bool2x1: UnicodeString;
    function Get_Bool2x2: UnicodeString;
    function Get_Bool2x3: UnicodeString;
    function Get_Bool2x4: UnicodeString;
    function Get_Bool3x1: UnicodeString;
    function Get_Bool3x2: UnicodeString;
    function Get_Bool3x3: UnicodeString;
    function Get_Bool3x4: UnicodeString;
    function Get_Bool4x1: UnicodeString;
    function Get_Bool4x2: UnicodeString;
    function Get_Bool4x3: UnicodeString;
    function Get_Bool4x4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x1: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float2x3: UnicodeString;
    function Get_Float2x4: UnicodeString;
    function Get_Float3x1: UnicodeString;
    function Get_Float3x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float3x4: UnicodeString;
    function Get_Float4x1: UnicodeString;
    function Get_Float4x2: UnicodeString;
    function Get_Float4x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Int2x1: UnicodeString;
    function Get_Int2x2: UnicodeString;
    function Get_Int2x3: UnicodeString;
    function Get_Int2x4: UnicodeString;
    function Get_Int3x1: UnicodeString;
    function Get_Int3x2: UnicodeString;
    function Get_Int3x3: UnicodeString;
    function Get_Int3x4: UnicodeString;
    function Get_Int4x1: UnicodeString;
    function Get_Int4x2: UnicodeString;
    function Get_Int4x3: UnicodeString;
    function Get_Int4x4: UnicodeString;
    function Get_Half: Double;
    function Get_Half2: UnicodeString;
    function Get_Half3: UnicodeString;
    function Get_Half4: UnicodeString;
    function Get_Half2x1: UnicodeString;
    function Get_Half2x2: UnicodeString;
    function Get_Half2x3: UnicodeString;
    function Get_Half2x4: UnicodeString;
    function Get_Half3x1: UnicodeString;
    function Get_Half3x2: UnicodeString;
    function Get_Half3x3: UnicodeString;
    function Get_Half3x4: UnicodeString;
    function Get_Half4x1: UnicodeString;
    function Get_Half4x2: UnicodeString;
    function Get_Half4x3: UnicodeString;
    function Get_Half4x4: UnicodeString;
    function Get_Fixed: Double;
    function Get_Fixed2: UnicodeString;
    function Get_Fixed3: UnicodeString;
    function Get_Fixed4: UnicodeString;
    function Get_Fixed2x1: UnicodeString;
    function Get_Fixed2x2: UnicodeString;
    function Get_Fixed2x3: UnicodeString;
    function Get_Fixed2x4: UnicodeString;
    function Get_Fixed3x1: UnicodeString;
    function Get_Fixed3x2: UnicodeString;
    function Get_Fixed3x3: UnicodeString;
    function Get_Fixed3x4: UnicodeString;
    function Get_Fixed4x1: UnicodeString;
    function Get_Fixed4x2: UnicodeString;
    function Get_Fixed4x3: UnicodeString;
    function Get_Fixed4x4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_String_: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLCg_array_type;
    function Get_Usertype: IXMLCg_user_type;
    procedure Set_Ref(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Bool2x1(Value: UnicodeString);
    procedure Set_Bool2x2(Value: UnicodeString);
    procedure Set_Bool2x3(Value: UnicodeString);
    procedure Set_Bool2x4(Value: UnicodeString);
    procedure Set_Bool3x1(Value: UnicodeString);
    procedure Set_Bool3x2(Value: UnicodeString);
    procedure Set_Bool3x3(Value: UnicodeString);
    procedure Set_Bool3x4(Value: UnicodeString);
    procedure Set_Bool4x1(Value: UnicodeString);
    procedure Set_Bool4x2(Value: UnicodeString);
    procedure Set_Bool4x3(Value: UnicodeString);
    procedure Set_Bool4x4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x1(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float2x3(Value: UnicodeString);
    procedure Set_Float2x4(Value: UnicodeString);
    procedure Set_Float3x1(Value: UnicodeString);
    procedure Set_Float3x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float3x4(Value: UnicodeString);
    procedure Set_Float4x1(Value: UnicodeString);
    procedure Set_Float4x2(Value: UnicodeString);
    procedure Set_Float4x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Int2x1(Value: UnicodeString);
    procedure Set_Int2x2(Value: UnicodeString);
    procedure Set_Int2x3(Value: UnicodeString);
    procedure Set_Int2x4(Value: UnicodeString);
    procedure Set_Int3x1(Value: UnicodeString);
    procedure Set_Int3x2(Value: UnicodeString);
    procedure Set_Int3x3(Value: UnicodeString);
    procedure Set_Int3x4(Value: UnicodeString);
    procedure Set_Int4x1(Value: UnicodeString);
    procedure Set_Int4x2(Value: UnicodeString);
    procedure Set_Int4x3(Value: UnicodeString);
    procedure Set_Int4x4(Value: UnicodeString);
    procedure Set_Half(Value: Double);
    procedure Set_Half2(Value: UnicodeString);
    procedure Set_Half3(Value: UnicodeString);
    procedure Set_Half4(Value: UnicodeString);
    procedure Set_Half2x1(Value: UnicodeString);
    procedure Set_Half2x2(Value: UnicodeString);
    procedure Set_Half2x3(Value: UnicodeString);
    procedure Set_Half2x4(Value: UnicodeString);
    procedure Set_Half3x1(Value: UnicodeString);
    procedure Set_Half3x2(Value: UnicodeString);
    procedure Set_Half3x3(Value: UnicodeString);
    procedure Set_Half3x4(Value: UnicodeString);
    procedure Set_Half4x1(Value: UnicodeString);
    procedure Set_Half4x2(Value: UnicodeString);
    procedure Set_Half4x3(Value: UnicodeString);
    procedure Set_Half4x4(Value: UnicodeString);
    procedure Set_Fixed(Value: Double);
    procedure Set_Fixed2(Value: UnicodeString);
    procedure Set_Fixed3(Value: UnicodeString);
    procedure Set_Fixed4(Value: UnicodeString);
    procedure Set_Fixed2x1(Value: UnicodeString);
    procedure Set_Fixed2x2(Value: UnicodeString);
    procedure Set_Fixed2x3(Value: UnicodeString);
    procedure Set_Fixed2x4(Value: UnicodeString);
    procedure Set_Fixed3x1(Value: UnicodeString);
    procedure Set_Fixed3x2(Value: UnicodeString);
    procedure Set_Fixed3x3(Value: UnicodeString);
    procedure Set_Fixed3x4(Value: UnicodeString);
    procedure Set_Fixed4x1(Value: UnicodeString);
    procedure Set_Fixed4x2(Value: UnicodeString);
    procedure Set_Fixed4x3(Value: UnicodeString);
    procedure Set_Fixed4x4(Value: UnicodeString);
    procedure Set_String_(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_cg_type_technique }

  TXMLProfile_cg_type_technique = class(TXMLNode, IXMLProfile_cg_type_technique)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FPass: IXMLCg_pass_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_cg_type_technique }
    function Get_Id: UnicodeString;
    function Get_Sid: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Pass: IXMLCg_pass_typeList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_cg_type_techniqueList }

  TXMLProfile_cg_type_techniqueList = class(TXMLNodeCollection, IXMLProfile_cg_type_techniqueList)
  protected
    { IXMLProfile_cg_type_techniqueList }
    function Add: IXMLProfile_cg_type_technique;
    function Insert(const Index: Integer): IXMLProfile_cg_type_technique;

    function Get_Item(Index: Integer): IXMLProfile_cg_type_technique;
  end;

{ TXMLCg_pass_type }

  TXMLCg_pass_type = class(TXMLNode, IXMLCg_pass_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLCg_pass_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_States: IXMLCg_pass_type_states;
    function Get_Program_: IXMLCg_pass_type_program;
    function Get_Evaluate: IXMLCg_pass_type_evaluate;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_pass_typeList }

  TXMLCg_pass_typeList = class(TXMLNodeCollection, IXMLCg_pass_typeList)
  protected
    { IXMLCg_pass_typeList }
    function Add: IXMLCg_pass_type;
    function Insert(const Index: Integer): IXMLCg_pass_type;

    function Get_Item(Index: Integer): IXMLCg_pass_type;
  end;

{ TXMLCg_pass_type_states }

  TXMLCg_pass_type_states = class(TXMLNode, IXMLCg_pass_type_states)
  protected
    { IXMLCg_pass_type_states }
    function Get_Alpha_func: IXMLAlpha_func;
    function Get_Blend_func: IXMLBlend_func;
    function Get_Blend_func_separate: IXMLBlend_func_separate;
    function Get_Blend_equation: IXMLBlend_equation;
    function Get_Blend_equation_separate: IXMLBlend_equation_separate;
    function Get_Color_material: IXMLColor_material;
    function Get_Cull_face: IXMLCull_face;
    function Get_Depth_func: IXMLDepth_func;
    function Get_Fog_mode: IXMLFog_mode;
    function Get_Fog_coord_src: IXMLFog_coord_src;
    function Get_Front_face: IXMLFront_face;
    function Get_Light_model_color_control: IXMLLight_model_color_control;
    function Get_Logic_op: IXMLLogic_op;
    function Get_Polygon_mode: IXMLPolygon_mode;
    function Get_Shade_model: IXMLShade_model;
    function Get_Stencil_func: IXMLStencil_func;
    function Get_Stencil_op: IXMLStencil_op;
    function Get_Stencil_func_separate: IXMLStencil_func_separate;
    function Get_Stencil_op_separate: IXMLStencil_op_separate;
    function Get_Stencil_mask_separate: IXMLStencil_mask_separate;
    function Get_Light_enable: IXMLLight_enable;
    function Get_Light_ambient: IXMLLight_ambient;
    function Get_Light_diffuse: IXMLLight_diffuse;
    function Get_Light_specular: IXMLLight_specular;
    function Get_Light_position: IXMLLight_position;
    function Get_Light_constant_attenuation: IXMLLight_constant_attenuation;
    function Get_Light_linear_attenuation: IXMLLight_linear_attenuation;
    function Get_Light_quadratic_attenuation: IXMLLight_quadratic_attenuation;
    function Get_Light_spot_cutoff: IXMLLight_spot_cutoff;
    function Get_Light_spot_direction: IXMLLight_spot_direction;
    function Get_Light_spot_exponent: IXMLLight_spot_exponent;
    function Get_Texture1D: IXMLTexture1D;
    function Get_Texture2D: IXMLTexture2D;
    function Get_Texture3D: IXMLTexture3D;
    function Get_TextureCUBE: IXMLTextureCUBE;
    function Get_TextureRECT: IXMLTextureRECT;
    function Get_TextureDEPTH: IXMLTextureDEPTH;
    function Get_Texture1D_enable: IXMLTexture1D_enable;
    function Get_Texture2D_enable: IXMLTexture2D_enable;
    function Get_Texture3D_enable: IXMLTexture3D_enable;
    function Get_TextureCUBE_enable: IXMLTextureCUBE_enable;
    function Get_TextureRECT_enable: IXMLTextureRECT_enable;
    function Get_TextureDEPTH_enable: IXMLTextureDEPTH_enable;
    function Get_Texture_env_color: IXMLTexture_env_color;
    function Get_Texture_env_mode: IXMLTexture_env_mode;
    function Get_Clip_plane: IXMLClip_plane;
    function Get_Clip_plane_enable: IXMLClip_plane_enable;
    function Get_Blend_color: IXMLBlend_color;
    function Get_Color_mask: IXMLColor_mask;
    function Get_Depth_bounds: IXMLDepth_bounds;
    function Get_Depth_mask: IXMLDepth_mask;
    function Get_Depth_range: IXMLDepth_range;
    function Get_Fog_density: IXMLFog_density;
    function Get_Fog_start: IXMLFog_start;
    function Get_Fog_end: IXMLFog_end;
    function Get_Fog_color: IXMLFog_color;
    function Get_Light_model_ambient: IXMLLight_model_ambient;
    function Get_Lighting_enable: IXMLLighting_enable;
    function Get_Line_stipple: IXMLLine_stipple;
    function Get_Line_width: IXMLLine_width;
    function Get_Material_ambient: IXMLMaterial_ambient;
    function Get_Material_diffuse: IXMLMaterial_diffuse;
    function Get_Material_emission: IXMLMaterial_emission;
    function Get_Material_shininess: IXMLMaterial_shininess;
    function Get_Material_specular: IXMLMaterial_specular;
    function Get_Model_view_matrix: IXMLModel_view_matrix;
    function Get_Point_distance_attenuation: IXMLPoint_distance_attenuation;
    function Get_Point_fade_threshold_size: IXMLPoint_fade_threshold_size;
    function Get_Point_size: IXMLPoint_size;
    function Get_Point_size_min: IXMLPoint_size_min;
    function Get_Point_size_max: IXMLPoint_size_max;
    function Get_Polygon_offset: IXMLPolygon_offset;
    function Get_Projection_matrix: IXMLProjection_matrix;
    function Get_Scissor: IXMLScissor;
    function Get_Stencil_mask: IXMLStencil_mask;
    function Get_Alpha_test_enable: IXMLAlpha_test_enable;
    function Get_Blend_enable: IXMLBlend_enable;
    function Get_Color_logic_op_enable: IXMLColor_logic_op_enable;
    function Get_Color_material_enable: IXMLColor_material_enable;
    function Get_Cull_face_enable: IXMLCull_face_enable;
    function Get_Depth_bounds_enable: IXMLDepth_bounds_enable;
    function Get_Depth_clamp_enable: IXMLDepth_clamp_enable;
    function Get_Depth_test_enable: IXMLDepth_test_enable;
    function Get_Dither_enable: IXMLDither_enable;
    function Get_Fog_enable: IXMLFog_enable;
    function Get_Light_model_local_viewer_enable: IXMLLight_model_local_viewer_enable;
    function Get_Light_model_two_side_enable: IXMLLight_model_two_side_enable;
    function Get_Line_smooth_enable: IXMLLine_smooth_enable;
    function Get_Line_stipple_enable: IXMLLine_stipple_enable;
    function Get_Logic_op_enable: IXMLLogic_op_enable;
    function Get_Multisample_enable: IXMLMultisample_enable;
    function Get_Normalize_enable: IXMLNormalize_enable;
    function Get_Point_smooth_enable: IXMLPoint_smooth_enable;
    function Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
    function Get_Polygon_offset_line_enable: IXMLPolygon_offset_line_enable;
    function Get_Polygon_offset_point_enable: IXMLPolygon_offset_point_enable;
    function Get_Polygon_smooth_enable: IXMLPolygon_smooth_enable;
    function Get_Polygon_stipple_enable: IXMLPolygon_stipple_enable;
    function Get_Rescale_normal_enable: IXMLRescale_normal_enable;
    function Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
    function Get_Sample_alpha_to_one_enable: IXMLSample_alpha_to_one_enable;
    function Get_Sample_coverage_enable: IXMLSample_coverage_enable;
    function Get_Scissor_test_enable: IXMLScissor_test_enable;
    function Get_Stencil_test_enable: IXMLStencil_test_enable;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_pass_type_program }

  TXMLCg_pass_type_program = class(TXMLNodeCollection, IXMLCg_pass_type_program)
  protected
    { IXMLCg_pass_type_program }
    function Get_Shader(Index: Integer): IXMLCg_pass_type_program_shader;
    function Add: IXMLCg_pass_type_program_shader;
    function Insert(const Index: Integer): IXMLCg_pass_type_program_shader;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_pass_type_program_shader }

  TXMLCg_pass_type_program_shader = class(TXMLNode, IXMLCg_pass_type_program_shader)
  private
    FCompiler: IXMLFx_target_typeList;
    FBind_uniform: IXMLCg_pass_type_program_shader_bind_uniformList;
  protected
    { IXMLCg_pass_type_program_shader }
    function Get_Stage: UnicodeString;
    function Get_Sources: IXMLCg_pass_type_program_shader_sources;
    function Get_Compiler: IXMLFx_target_typeList;
    function Get_Bind_uniform: IXMLCg_pass_type_program_shader_bind_uniformList;
    procedure Set_Stage(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_pass_type_program_shader_sources }

  TXMLCg_pass_type_program_shader_sources = class(TXMLFx_sources_type, IXMLCg_pass_type_program_shader_sources)
  protected
    { IXMLCg_pass_type_program_shader_sources }
    function Get_Entry: UnicodeString;
    procedure Set_Entry(Value: UnicodeString);
  end;

{ TXMLCg_pass_type_program_shader_bind_uniform }

  TXMLCg_pass_type_program_shader_bind_uniform = class(TXMLNode, IXMLCg_pass_type_program_shader_bind_uniform)
  protected
    { IXMLCg_pass_type_program_shader_bind_uniform }
    function Get_Symbol: UnicodeString;
    function Get_Param: IXMLCg_pass_type_program_shader_bind_uniform_param;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Bool2x1: UnicodeString;
    function Get_Bool2x2: UnicodeString;
    function Get_Bool2x3: UnicodeString;
    function Get_Bool2x4: UnicodeString;
    function Get_Bool3x1: UnicodeString;
    function Get_Bool3x2: UnicodeString;
    function Get_Bool3x3: UnicodeString;
    function Get_Bool3x4: UnicodeString;
    function Get_Bool4x1: UnicodeString;
    function Get_Bool4x2: UnicodeString;
    function Get_Bool4x3: UnicodeString;
    function Get_Bool4x4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float2x1: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float2x3: UnicodeString;
    function Get_Float2x4: UnicodeString;
    function Get_Float3x1: UnicodeString;
    function Get_Float3x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float3x4: UnicodeString;
    function Get_Float4x1: UnicodeString;
    function Get_Float4x2: UnicodeString;
    function Get_Float4x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Int2x1: UnicodeString;
    function Get_Int2x2: UnicodeString;
    function Get_Int2x3: UnicodeString;
    function Get_Int2x4: UnicodeString;
    function Get_Int3x1: UnicodeString;
    function Get_Int3x2: UnicodeString;
    function Get_Int3x3: UnicodeString;
    function Get_Int3x4: UnicodeString;
    function Get_Int4x1: UnicodeString;
    function Get_Int4x2: UnicodeString;
    function Get_Int4x3: UnicodeString;
    function Get_Int4x4: UnicodeString;
    function Get_Half: Double;
    function Get_Half2: UnicodeString;
    function Get_Half3: UnicodeString;
    function Get_Half4: UnicodeString;
    function Get_Half2x1: UnicodeString;
    function Get_Half2x2: UnicodeString;
    function Get_Half2x3: UnicodeString;
    function Get_Half2x4: UnicodeString;
    function Get_Half3x1: UnicodeString;
    function Get_Half3x2: UnicodeString;
    function Get_Half3x3: UnicodeString;
    function Get_Half3x4: UnicodeString;
    function Get_Half4x1: UnicodeString;
    function Get_Half4x2: UnicodeString;
    function Get_Half4x3: UnicodeString;
    function Get_Half4x4: UnicodeString;
    function Get_Fixed: Double;
    function Get_Fixed2: UnicodeString;
    function Get_Fixed3: UnicodeString;
    function Get_Fixed4: UnicodeString;
    function Get_Fixed2x1: UnicodeString;
    function Get_Fixed2x2: UnicodeString;
    function Get_Fixed2x3: UnicodeString;
    function Get_Fixed2x4: UnicodeString;
    function Get_Fixed3x1: UnicodeString;
    function Get_Fixed3x2: UnicodeString;
    function Get_Fixed3x3: UnicodeString;
    function Get_Fixed3x4: UnicodeString;
    function Get_Fixed4x1: UnicodeString;
    function Get_Fixed4x2: UnicodeString;
    function Get_Fixed4x3: UnicodeString;
    function Get_Fixed4x4: UnicodeString;
    function Get_Sampler1D: IXMLFx_sampler1D_type;
    function Get_Sampler2D: IXMLFx_sampler2D_type;
    function Get_Sampler3D: IXMLFx_sampler3D_type;
    function Get_SamplerRECT: IXMLFx_samplerRECT_type;
    function Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
    function Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
    function Get_String_: UnicodeString;
    function Get_Enum: UnicodeString;
    function Get_Array_: IXMLCg_array_type;
    function Get_Usertype: IXMLCg_user_type;
    procedure Set_Symbol(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Bool2x1(Value: UnicodeString);
    procedure Set_Bool2x2(Value: UnicodeString);
    procedure Set_Bool2x3(Value: UnicodeString);
    procedure Set_Bool2x4(Value: UnicodeString);
    procedure Set_Bool3x1(Value: UnicodeString);
    procedure Set_Bool3x2(Value: UnicodeString);
    procedure Set_Bool3x3(Value: UnicodeString);
    procedure Set_Bool3x4(Value: UnicodeString);
    procedure Set_Bool4x1(Value: UnicodeString);
    procedure Set_Bool4x2(Value: UnicodeString);
    procedure Set_Bool4x3(Value: UnicodeString);
    procedure Set_Bool4x4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float2x1(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float2x3(Value: UnicodeString);
    procedure Set_Float2x4(Value: UnicodeString);
    procedure Set_Float3x1(Value: UnicodeString);
    procedure Set_Float3x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float3x4(Value: UnicodeString);
    procedure Set_Float4x1(Value: UnicodeString);
    procedure Set_Float4x2(Value: UnicodeString);
    procedure Set_Float4x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Int2x1(Value: UnicodeString);
    procedure Set_Int2x2(Value: UnicodeString);
    procedure Set_Int2x3(Value: UnicodeString);
    procedure Set_Int2x4(Value: UnicodeString);
    procedure Set_Int3x1(Value: UnicodeString);
    procedure Set_Int3x2(Value: UnicodeString);
    procedure Set_Int3x3(Value: UnicodeString);
    procedure Set_Int3x4(Value: UnicodeString);
    procedure Set_Int4x1(Value: UnicodeString);
    procedure Set_Int4x2(Value: UnicodeString);
    procedure Set_Int4x3(Value: UnicodeString);
    procedure Set_Int4x4(Value: UnicodeString);
    procedure Set_Half(Value: Double);
    procedure Set_Half2(Value: UnicodeString);
    procedure Set_Half3(Value: UnicodeString);
    procedure Set_Half4(Value: UnicodeString);
    procedure Set_Half2x1(Value: UnicodeString);
    procedure Set_Half2x2(Value: UnicodeString);
    procedure Set_Half2x3(Value: UnicodeString);
    procedure Set_Half2x4(Value: UnicodeString);
    procedure Set_Half3x1(Value: UnicodeString);
    procedure Set_Half3x2(Value: UnicodeString);
    procedure Set_Half3x3(Value: UnicodeString);
    procedure Set_Half3x4(Value: UnicodeString);
    procedure Set_Half4x1(Value: UnicodeString);
    procedure Set_Half4x2(Value: UnicodeString);
    procedure Set_Half4x3(Value: UnicodeString);
    procedure Set_Half4x4(Value: UnicodeString);
    procedure Set_Fixed(Value: Double);
    procedure Set_Fixed2(Value: UnicodeString);
    procedure Set_Fixed3(Value: UnicodeString);
    procedure Set_Fixed4(Value: UnicodeString);
    procedure Set_Fixed2x1(Value: UnicodeString);
    procedure Set_Fixed2x2(Value: UnicodeString);
    procedure Set_Fixed2x3(Value: UnicodeString);
    procedure Set_Fixed2x4(Value: UnicodeString);
    procedure Set_Fixed3x1(Value: UnicodeString);
    procedure Set_Fixed3x2(Value: UnicodeString);
    procedure Set_Fixed3x3(Value: UnicodeString);
    procedure Set_Fixed3x4(Value: UnicodeString);
    procedure Set_Fixed4x1(Value: UnicodeString);
    procedure Set_Fixed4x2(Value: UnicodeString);
    procedure Set_Fixed4x3(Value: UnicodeString);
    procedure Set_Fixed4x4(Value: UnicodeString);
    procedure Set_String_(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCg_pass_type_program_shader_bind_uniformList }

  TXMLCg_pass_type_program_shader_bind_uniformList = class(TXMLNodeCollection, IXMLCg_pass_type_program_shader_bind_uniformList)
  protected
    { IXMLCg_pass_type_program_shader_bind_uniformList }
    function Add: IXMLCg_pass_type_program_shader_bind_uniform;
    function Insert(const Index: Integer): IXMLCg_pass_type_program_shader_bind_uniform;

    function Get_Item(Index: Integer): IXMLCg_pass_type_program_shader_bind_uniform;
  end;

{ TXMLCg_pass_type_program_shader_bind_uniform_param }

  TXMLCg_pass_type_program_shader_bind_uniform_param = class(TXMLNode, IXMLCg_pass_type_program_shader_bind_uniform_param)
  protected
    { IXMLCg_pass_type_program_shader_bind_uniform_param }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLCg_pass_type_evaluate }

  TXMLCg_pass_type_evaluate = class(TXMLNode, IXMLCg_pass_type_evaluate)
  private
    FColor_target: IXMLFx_colortarget_typeList;
    FDepth_target: IXMLFx_depthtarget_typeList;
    FStencil_target: IXMLFx_stenciltarget_typeList;
    FColor_clear: IXMLFx_clearcolor_typeList;
    FDepth_clear: IXMLFx_cleardepth_typeList;
    FStencil_clear: IXMLFx_clearstencil_typeList;
  protected
    { IXMLCg_pass_type_evaluate }
    function Get_Color_target: IXMLFx_colortarget_typeList;
    function Get_Depth_target: IXMLFx_depthtarget_typeList;
    function Get_Stencil_target: IXMLFx_stenciltarget_typeList;
    function Get_Color_clear: IXMLFx_clearcolor_typeList;
    function Get_Depth_clear: IXMLFx_cleardepth_typeList;
    function Get_Stencil_clear: IXMLFx_clearstencil_typeList;
    function Get_Draw: UnicodeString;
    procedure Set_Draw(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_gles_type }

  TXMLProfile_gles_type = class(TXMLNode, IXMLProfile_gles_type)
  private
    FNewparam: IXMLGles_newparam_typeList;
    FTechnique: IXMLProfile_gles_type_techniqueList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_gles_type }
    function Get_Id: UnicodeString;
    function Get_Platform: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Newparam: IXMLGles_newparam_typeList;
    function Get_Technique: IXMLProfile_gles_type_techniqueList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Platform(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_newparam_type }

  TXMLGles_newparam_type = class(TXMLNode, IXMLGles_newparam_type)
  private
    FAnnotate: IXMLFx_annotate_typeList;
  protected
    { IXMLGles_newparam_type }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Semantic: UnicodeString;
    function Get_Modifier: UnicodeString;
    function Get_Bool: Boolean;
    function Get_Bool2: UnicodeString;
    function Get_Bool3: UnicodeString;
    function Get_Bool4: UnicodeString;
    function Get_Int: Int64;
    function Get_Int2: UnicodeString;
    function Get_Int3: UnicodeString;
    function Get_Int4: UnicodeString;
    function Get_Float: Double;
    function Get_Float2: UnicodeString;
    function Get_Float3: UnicodeString;
    function Get_Float4: UnicodeString;
    function Get_Float1x1: Double;
    function Get_Float1x2: UnicodeString;
    function Get_Float1x3: UnicodeString;
    function Get_Float1x4: UnicodeString;
    function Get_Float2x1: UnicodeString;
    function Get_Float2x2: UnicodeString;
    function Get_Float2x3: UnicodeString;
    function Get_Float2x4: UnicodeString;
    function Get_Float3x1: UnicodeString;
    function Get_Float3x2: UnicodeString;
    function Get_Float3x3: UnicodeString;
    function Get_Float3x4: UnicodeString;
    function Get_Float4x1: UnicodeString;
    function Get_Float4x2: UnicodeString;
    function Get_Float4x3: UnicodeString;
    function Get_Float4x4: UnicodeString;
    function Get_Sampler2D: IXMLGles_sampler_type;
    function Get_Enum: UnicodeString;
    procedure Set_Sid(Value: UnicodeString);
    procedure Set_Semantic(Value: UnicodeString);
    procedure Set_Modifier(Value: UnicodeString);
    procedure Set_Bool(Value: Boolean);
    procedure Set_Bool2(Value: UnicodeString);
    procedure Set_Bool3(Value: UnicodeString);
    procedure Set_Bool4(Value: UnicodeString);
    procedure Set_Int(Value: Int64);
    procedure Set_Int2(Value: UnicodeString);
    procedure Set_Int3(Value: UnicodeString);
    procedure Set_Int4(Value: UnicodeString);
    procedure Set_Float(Value: Double);
    procedure Set_Float2(Value: UnicodeString);
    procedure Set_Float3(Value: UnicodeString);
    procedure Set_Float4(Value: UnicodeString);
    procedure Set_Float1x1(Value: Double);
    procedure Set_Float1x2(Value: UnicodeString);
    procedure Set_Float1x3(Value: UnicodeString);
    procedure Set_Float1x4(Value: UnicodeString);
    procedure Set_Float2x1(Value: UnicodeString);
    procedure Set_Float2x2(Value: UnicodeString);
    procedure Set_Float2x3(Value: UnicodeString);
    procedure Set_Float2x4(Value: UnicodeString);
    procedure Set_Float3x1(Value: UnicodeString);
    procedure Set_Float3x2(Value: UnicodeString);
    procedure Set_Float3x3(Value: UnicodeString);
    procedure Set_Float3x4(Value: UnicodeString);
    procedure Set_Float4x1(Value: UnicodeString);
    procedure Set_Float4x2(Value: UnicodeString);
    procedure Set_Float4x3(Value: UnicodeString);
    procedure Set_Float4x4(Value: UnicodeString);
    procedure Set_Enum(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_newparam_typeList }

  TXMLGles_newparam_typeList = class(TXMLNodeCollection, IXMLGles_newparam_typeList)
  protected
    { IXMLGles_newparam_typeList }
    function Add: IXMLGles_newparam_type;
    function Insert(const Index: Integer): IXMLGles_newparam_type;

    function Get_Item(Index: Integer): IXMLGles_newparam_type;
  end;

{ TXMLGles_sampler_type }

  TXMLGles_sampler_type = class(TXMLNode, IXMLGles_sampler_type)
  private
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLGles_sampler_type }
    function Get_Instance_image: IXMLInstance_image_type;
    function Get_Texcoord: IXMLGles_sampler_type_texcoord;
    function Get_Wrap_s: UnicodeString;
    function Get_Wrap_t: UnicodeString;
    function Get_Minfilter: UnicodeString;
    function Get_Magfilter: UnicodeString;
    function Get_Mipfilter: UnicodeString;
    function Get_Mip_max_level: Byte;
    function Get_Mip_bias: Single;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Wrap_s(Value: UnicodeString);
    procedure Set_Wrap_t(Value: UnicodeString);
    procedure Set_Minfilter(Value: UnicodeString);
    procedure Set_Magfilter(Value: UnicodeString);
    procedure Set_Mipfilter(Value: UnicodeString);
    procedure Set_Mip_max_level(Value: Byte);
    procedure Set_Mip_bias(Value: Single);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_sampler_type_texcoord }

  TXMLGles_sampler_type_texcoord = class(TXMLNode, IXMLGles_sampler_type_texcoord)
  protected
    { IXMLGles_sampler_type_texcoord }
    function Get_Semantic: UnicodeString;
    procedure Set_Semantic(Value: UnicodeString);
  end;

{ TXMLProfile_gles_type_technique }

  TXMLProfile_gles_type_technique = class(TXMLNode, IXMLProfile_gles_type_technique)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FPass: IXMLProfile_gles_type_technique_passList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_gles_type_technique }
    function Get_Id: UnicodeString;
    function Get_Sid: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_Pass: IXMLProfile_gles_type_technique_passList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_gles_type_techniqueList }

  TXMLProfile_gles_type_techniqueList = class(TXMLNodeCollection, IXMLProfile_gles_type_techniqueList)
  protected
    { IXMLProfile_gles_type_techniqueList }
    function Add: IXMLProfile_gles_type_technique;
    function Insert(const Index: Integer): IXMLProfile_gles_type_technique;

    function Get_Item(Index: Integer): IXMLProfile_gles_type_technique;
  end;

{ TXMLProfile_gles_type_technique_pass }

  TXMLProfile_gles_type_technique_pass = class(TXMLNode, IXMLProfile_gles_type_technique_pass)
  private
    FAnnotate: IXMLFx_annotate_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_gles_type_technique_pass }
    function Get_Sid: UnicodeString;
    function Get_Annotate: IXMLFx_annotate_typeList;
    function Get_States: IXMLProfile_gles_type_technique_pass_states;
    function Get_Evaluate: IXMLProfile_gles_type_technique_pass_evaluate;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_gles_type_technique_passList }

  TXMLProfile_gles_type_technique_passList = class(TXMLNodeCollection, IXMLProfile_gles_type_technique_passList)
  protected
    { IXMLProfile_gles_type_technique_passList }
    function Add: IXMLProfile_gles_type_technique_pass;
    function Insert(const Index: Integer): IXMLProfile_gles_type_technique_pass;

    function Get_Item(Index: Integer): IXMLProfile_gles_type_technique_pass;
  end;

{ TXMLProfile_gles_type_technique_pass_states }

  TXMLProfile_gles_type_technique_pass_states = class(TXMLNode, IXMLProfile_gles_type_technique_pass_states)
  protected
    { IXMLProfile_gles_type_technique_pass_states }
    function Get_Alpha_func: IXMLAlpha_func;
    function Get_Blend_func: IXMLBlend_func;
    function Get_Clip_plane: IXMLClip_plane;
    function Get_Color_mask: IXMLColor_mask;
    function Get_Cull_face: IXMLCull_face;
    function Get_Depth_func: IXMLDepth_func;
    function Get_Depth_mask: IXMLDepth_mask;
    function Get_Depth_range: IXMLDepth_range;
    function Get_Fog_color: IXMLFog_color;
    function Get_Fog_density: IXMLFog_density;
    function Get_Fog_mode: IXMLFog_mode;
    function Get_Fog_start: IXMLFog_start;
    function Get_Fog_end: IXMLFog_end;
    function Get_Front_face: IXMLFront_face;
    function Get_Logic_op: IXMLLogic_op;
    function Get_Light_ambient: IXMLLight_ambient;
    function Get_Light_diffuse: IXMLLight_diffuse;
    function Get_Light_specular: IXMLLight_specular;
    function Get_Light_position: IXMLLight_position;
    function Get_Light_constant_attenuation: IXMLLight_constant_attenuation;
    function Get_Light_linear_attenuation: IXMLLight_linear_attenuation;
    function Get_Light_quadratic_attenuation: IXMLLight_quadratic_attenuation;
    function Get_Light_spot_cutoff: IXMLLight_spot_cutoff;
    function Get_Light_spot_direction: IXMLLight_spot_direction;
    function Get_Light_spot_exponent: IXMLLight_spot_exponent;
    function Get_Light_model_ambient: IXMLLight_model_ambient;
    function Get_Line_width: IXMLLine_width;
    function Get_Material_ambient: IXMLMaterial_ambient;
    function Get_Material_diffuse: IXMLMaterial_diffuse;
    function Get_Material_emission: IXMLMaterial_emission;
    function Get_Material_shininess: IXMLMaterial_shininess;
    function Get_Material_specular: IXMLMaterial_specular;
    function Get_Model_view_matrix: IXMLModel_view_matrix;
    function Get_Point_distance_attenuation: IXMLPoint_distance_attenuation;
    function Get_Point_fade_threshold_size: IXMLPoint_fade_threshold_size;
    function Get_Point_size: IXMLPoint_size;
    function Get_Point_size_min: IXMLPoint_size_min;
    function Get_Point_size_max: IXMLPoint_size_max;
    function Get_Polygon_offset: IXMLPolygon_offset;
    function Get_Projection_matrix: IXMLProjection_matrix;
    function Get_Scissor: IXMLScissor;
    function Get_Shade_model: IXMLShade_model;
    function Get_Stencil_func: IXMLStencil_func;
    function Get_Stencil_mask: IXMLStencil_mask;
    function Get_Stencil_op: IXMLStencil_op;
    function Get_Texture_pipeline: IXMLTexture_pipeline;
    function Get_Alpha_test_enable: IXMLAlpha_test_enable;
    function Get_Blend_enable: IXMLBlend_enable;
    function Get_Clip_plane_enable: IXMLClip_plane_enable;
    function Get_Color_logic_op_enable: IXMLColor_logic_op_enable;
    function Get_Color_material_enable: IXMLColor_material_enable;
    function Get_Cull_face_enable: IXMLCull_face_enable;
    function Get_Depth_test_enable: IXMLDepth_test_enable;
    function Get_Dither_enable: IXMLDither_enable;
    function Get_Fog_enable: IXMLFog_enable;
    function Get_Light_enable: IXMLLight_enable;
    function Get_Lighting_enable: IXMLLighting_enable;
    function Get_Light_model_two_side_enable: IXMLLight_model_two_side_enable;
    function Get_Line_smooth_enable: IXMLLine_smooth_enable;
    function Get_Multisample_enable: IXMLMultisample_enable;
    function Get_Normalize_enable: IXMLNormalize_enable;
    function Get_Point_smooth_enable: IXMLPoint_smooth_enable;
    function Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
    function Get_Rescale_normal_enable: IXMLRescale_normal_enable;
    function Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
    function Get_Sample_alpha_to_one_enable: IXMLSample_alpha_to_one_enable;
    function Get_Sample_coverage_enable: IXMLSample_coverage_enable;
    function Get_Scissor_test_enable: IXMLScissor_test_enable;
    function Get_Stencil_test_enable: IXMLStencil_test_enable;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLTexture_pipeline }

  TXMLTexture_pipeline = class(TXMLNode, IXMLTexture_pipeline)
  protected
    { IXMLTexture_pipeline }
    function Get_Value: IXMLGles_texture_pipeline_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_texture_pipeline_type }

  TXMLGles_texture_pipeline_type = class(TXMLNode, IXMLGles_texture_pipeline_type)
  private
    FTexcombiner: IXMLGles_texcombiner_command_typeList;
    FTexenv: IXMLGles_texenv_command_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLGles_texture_pipeline_type }
    function Get_Sid: UnicodeString;
    function Get_Texcombiner: IXMLGles_texcombiner_command_typeList;
    function Get_Texenv: IXMLGles_texenv_command_typeList;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_texcombiner_command_type }

  TXMLGles_texcombiner_command_type = class(TXMLNode, IXMLGles_texcombiner_command_type)
  protected
    { IXMLGles_texcombiner_command_type }
    function Get_Constant: IXMLGles_texture_constant_type;
    function Get_RGB: IXMLGles_texcombiner_command_rgb_type;
    function Get_Alpha: IXMLGles_texcombiner_command_alpha_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_texcombiner_command_typeList }

  TXMLGles_texcombiner_command_typeList = class(TXMLNodeCollection, IXMLGles_texcombiner_command_typeList)
  protected
    { IXMLGles_texcombiner_command_typeList }
    function Add: IXMLGles_texcombiner_command_type;
    function Insert(const Index: Integer): IXMLGles_texcombiner_command_type;

    function Get_Item(Index: Integer): IXMLGles_texcombiner_command_type;
  end;

{ TXMLGles_texture_constant_type }

  TXMLGles_texture_constant_type = class(TXMLNode, IXMLGles_texture_constant_type)
  protected
    { IXMLGles_texture_constant_type }
    function Get_Value: UnicodeString;
    function Get_Param: UnicodeString;
    procedure Set_Value(Value: UnicodeString);
    procedure Set_Param(Value: UnicodeString);
  end;

{ TXMLGles_texcombiner_command_rgb_type }

  TXMLGles_texcombiner_command_rgb_type = class(TXMLNodeCollection, IXMLGles_texcombiner_command_rgb_type)
  protected
    { IXMLGles_texcombiner_command_rgb_type }
    function Get_Operator_: UnicodeString;
    function Get_Scale: Single;
    function Get_Argument(Index: Integer): IXMLGles_texcombiner_argument_rgb_type;
    procedure Set_Operator_(Value: UnicodeString);
    procedure Set_Scale(Value: Single);
    function Add: IXMLGles_texcombiner_argument_rgb_type;
    function Insert(const Index: Integer): IXMLGles_texcombiner_argument_rgb_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_texcombiner_argument_rgb_type }

  TXMLGles_texcombiner_argument_rgb_type = class(TXMLNode, IXMLGles_texcombiner_argument_rgb_type)
  protected
    { IXMLGles_texcombiner_argument_rgb_type }
    function Get_Source: UnicodeString;
    function Get_Operand: UnicodeString;
    function Get_Sampler: UnicodeString;
    procedure Set_Source(Value: UnicodeString);
    procedure Set_Operand(Value: UnicodeString);
    procedure Set_Sampler(Value: UnicodeString);
  end;

{ TXMLGles_texcombiner_command_alpha_type }

  TXMLGles_texcombiner_command_alpha_type = class(TXMLNodeCollection, IXMLGles_texcombiner_command_alpha_type)
  protected
    { IXMLGles_texcombiner_command_alpha_type }
    function Get_Operator_: UnicodeString;
    function Get_Scale: Single;
    function Get_Argument(Index: Integer): IXMLGles_texcombiner_argument_alpha_type;
    procedure Set_Operator_(Value: UnicodeString);
    procedure Set_Scale(Value: Single);
    function Add: IXMLGles_texcombiner_argument_alpha_type;
    function Insert(const Index: Integer): IXMLGles_texcombiner_argument_alpha_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_texcombiner_argument_alpha_type }

  TXMLGles_texcombiner_argument_alpha_type = class(TXMLNode, IXMLGles_texcombiner_argument_alpha_type)
  protected
    { IXMLGles_texcombiner_argument_alpha_type }
    function Get_Source: UnicodeString;
    function Get_Operand: UnicodeString;
    function Get_Sampler: UnicodeString;
    procedure Set_Source(Value: UnicodeString);
    procedure Set_Operand(Value: UnicodeString);
    procedure Set_Sampler(Value: UnicodeString);
  end;

{ TXMLGles_texenv_command_type }

  TXMLGles_texenv_command_type = class(TXMLNode, IXMLGles_texenv_command_type)
  protected
    { IXMLGles_texenv_command_type }
    function Get_Operator_: UnicodeString;
    function Get_Sampler: UnicodeString;
    function Get_Constant: IXMLGles_texture_constant_type;
    procedure Set_Operator_(Value: UnicodeString);
    procedure Set_Sampler(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGles_texenv_command_typeList }

  TXMLGles_texenv_command_typeList = class(TXMLNodeCollection, IXMLGles_texenv_command_typeList)
  protected
    { IXMLGles_texenv_command_typeList }
    function Add: IXMLGles_texenv_command_type;
    function Insert(const Index: Integer): IXMLGles_texenv_command_type;

    function Get_Item(Index: Integer): IXMLGles_texenv_command_type;
  end;

{ TXMLProfile_gles_type_technique_pass_evaluate }

  TXMLProfile_gles_type_technique_pass_evaluate = class(TXMLNode, IXMLProfile_gles_type_technique_pass_evaluate)
  private
    FColor_target: IXMLFx_colortarget_typeList;
    FDepth_target: IXMLFx_depthtarget_typeList;
    FStencil_target: IXMLFx_stenciltarget_typeList;
    FColor_clear: IXMLFx_clearcolor_typeList;
    FDepth_clear: IXMLFx_cleardepth_typeList;
    FStencil_clear: IXMLFx_clearstencil_typeList;
  protected
    { IXMLProfile_gles_type_technique_pass_evaluate }
    function Get_Color_target: IXMLFx_colortarget_typeList;
    function Get_Depth_target: IXMLFx_depthtarget_typeList;
    function Get_Stencil_target: IXMLFx_stenciltarget_typeList;
    function Get_Color_clear: IXMLFx_clearcolor_typeList;
    function Get_Depth_clear: IXMLFx_cleardepth_typeList;
    function Get_Stencil_clear: IXMLFx_clearstencil_typeList;
    function Get_Draw: UnicodeString;
    procedure Set_Draw(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;




{ TXMLProfile_common_type }

  TXMLProfile_common_type = class(TXMLNode, IXMLProfile_common_type)
  private
    FNewparam: IXMLFx_common_newparam_typeList;
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_common_type }
    function Get_Id: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Newparam: IXMLFx_common_newparam_typeList;
    function Get_Technique: IXMLProfile_common_type_technique;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

  { TXMLProfile_bridge_type }

  TXMLProfile_bridge_type = class(TXMLNode, IXMLProfile_bridge_type)
  private
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_bridge_type }
    function Get_Id: UnicodeString;
    function Get_Platform: UnicodeString;
    function Get_Url: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Platform(Value: UnicodeString);
    procedure Set_Url(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;
  { TXMLProfile_common_type_technique }

  TXMLProfile_common_type_technique = class(TXMLNode, IXMLProfile_common_type_technique)
  private
    FExtra: IXMLExtra_typeList;
  protected
    { IXMLProfile_common_type_technique }
    function Get_Id: UnicodeString;
    function Get_Sid: UnicodeString;
    function Get_Asset: IXMLAsset_type;
    function Get_Constant: IXMLProfile_common_type_technique_constant;
    function Get_Lambert: IXMLProfile_common_type_technique_lambert;
    function Get_Phong: IXMLProfile_common_type_technique_phong;
    function Get_Blinn: IXMLProfile_common_type_technique_blinn;
    function Get_Extra: IXMLExtra_typeList;
    procedure Set_Id(Value: UnicodeString);
    procedure Set_Sid(Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_common_type_technique_constant }

  TXMLProfile_common_type_technique_constant = class(TXMLNode, IXMLProfile_common_type_technique_constant)
  protected
    { IXMLProfile_common_type_technique_constant }
    function Get_Emission: IXMLFx_common_color_or_texture_type;
    function Get_Reflective: IXMLFx_common_color_or_texture_type;
    function Get_Reflectivity: IXMLFx_common_float_or_param_type;
    function Get_Transparent: IXMLFx_common_transparent_type;
    function Get_Transparency: IXMLFx_common_float_or_param_type;
    function Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
  public
    procedure AfterConstruction; override;
  end;
  { TXMLFx_common_color_or_texture_type }

  TXMLFx_common_color_or_texture_type = class(TXMLNode, IXMLFx_common_color_or_texture_type)
  protected
    { IXMLFx_common_color_or_texture_type }
    function Get_Color: IXMLFx_common_color_or_texture_type_color;
    function Get_Param: IXMLFx_common_color_or_texture_type_param;
    function Get_Texture: IXMLFx_common_color_or_texture_type_texture;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_common_color_or_texture_type_color }

  TXMLFx_common_color_or_texture_type_color = class(TXMLNode, IXMLFx_common_color_or_texture_type_color)
  protected
    { IXMLFx_common_color_or_texture_type_color }
    function Get_Sid: UnicodeString;
    procedure Set_Sid(Value: UnicodeString);
  end;

{ TXMLFx_common_color_or_texture_type_param }

  TXMLFx_common_color_or_texture_type_param = class(TXMLNode, IXMLFx_common_color_or_texture_type_param)
  protected
    { IXMLFx_common_color_or_texture_type_param }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLFx_common_color_or_texture_type_texture }

  TXMLFx_common_color_or_texture_type_texture = class(TXMLNodeCollection, IXMLFx_common_color_or_texture_type_texture)
  protected
    { IXMLFx_common_color_or_texture_type_texture }
    function Get_Texture: UnicodeString;
    function Get_Texcoord: UnicodeString;
    function Get_Extra(Index: Integer): IXMLExtra_type;
    procedure Set_Texture(Value: UnicodeString);
    procedure Set_Texcoord(Value: UnicodeString);
    function Add: IXMLExtra_type;
    function Insert(const Index: Integer): IXMLExtra_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_common_float_or_param_type }

  TXMLFx_common_float_or_param_type = class(TXMLNode, IXMLFx_common_float_or_param_type)
  protected
    { IXMLFx_common_float_or_param_type }
    function Get_Float: IXMLFx_common_float_or_param_type_float;
    function Get_Param: IXMLFx_common_float_or_param_type_param;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLFx_common_float_or_param_type_float }

  TXMLFx_common_float_or_param_type_float = class(TXMLNode, IXMLFx_common_float_or_param_type_float)
  protected
    { IXMLFx_common_float_or_param_type_float }
    function Get_Sid: UnicodeString;
    procedure Set_Sid(Value: UnicodeString);
  end;

{ TXMLFx_common_float_or_param_type_param }

  TXMLFx_common_float_or_param_type_param = class(TXMLNode, IXMLFx_common_float_or_param_type_param)
  protected
    { IXMLFx_common_float_or_param_type_param }
    function Get_Ref: UnicodeString;
    procedure Set_Ref(Value: UnicodeString);
  end;

{ TXMLFx_common_transparent_type }

  TXMLFx_common_transparent_type = class(TXMLFx_common_color_or_texture_type, IXMLFx_common_transparent_type)
  protected
    { IXMLFx_common_transparent_type }
    function Get_Opaque: UnicodeString;
    procedure Set_Opaque(Value: UnicodeString);
  end;

{ TXMLProfile_common_type_technique_lambert }

  TXMLProfile_common_type_technique_lambert = class(TXMLNode, IXMLProfile_common_type_technique_lambert)
  protected
    { IXMLProfile_common_type_technique_lambert }
    function Get_Emission: IXMLFx_common_color_or_texture_type;
    function Get_Ambient: IXMLFx_common_color_or_texture_type;
    function Get_Diffuse: IXMLFx_common_color_or_texture_type;
    function Get_Reflective: IXMLFx_common_color_or_texture_type;
    function Get_Reflectivity: IXMLFx_common_float_or_param_type;
    function Get_Transparent: IXMLFx_common_transparent_type;
    function Get_Transparency: IXMLFx_common_float_or_param_type;
    function Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_common_type_technique_phong }

  TXMLProfile_common_type_technique_phong = class(TXMLNode, IXMLProfile_common_type_technique_phong)
  protected
    { IXMLProfile_common_type_technique_phong }
    function Get_Emission: IXMLFx_common_color_or_texture_type;
    function Get_Ambient: IXMLFx_common_color_or_texture_type;
    function Get_Diffuse: IXMLFx_common_color_or_texture_type;
    function Get_Specular: IXMLFx_common_color_or_texture_type;
    function Get_Shininess: IXMLFx_common_float_or_param_type;
    function Get_Reflective: IXMLFx_common_color_or_texture_type;
    function Get_Reflectivity: IXMLFx_common_float_or_param_type;
    function Get_Transparent: IXMLFx_common_transparent_type;
    function Get_Transparency: IXMLFx_common_float_or_param_type;
    function Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLProfile_common_type_technique_blinn }

  TXMLProfile_common_type_technique_blinn = class(TXMLNode, IXMLProfile_common_type_technique_blinn)
  protected
    { IXMLProfile_common_type_technique_blinn }
    function Get_Emission: IXMLFx_common_color_or_texture_type;
    function Get_Ambient: IXMLFx_common_color_or_texture_type;
    function Get_Diffuse: IXMLFx_common_color_or_texture_type;
    function Get_Specular: IXMLFx_common_color_or_texture_type;
    function Get_Shininess: IXMLFx_common_float_or_param_type;
    function Get_Reflective: IXMLFx_common_color_or_texture_type;
    function Get_Reflectivity: IXMLFx_common_float_or_param_type;
    function Get_Transparent: IXMLFx_common_transparent_type;
    function Get_Transparency: IXMLFx_common_float_or_param_type;
    function Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
  public
    procedure AfterConstruction; override;
  end;



implementation
//uses FMX.DAE.Schema;





 { TXMLProfile_gles2_type }

procedure TXMLProfile_gles2_type.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('include', TXMLFx_include_type);
  RegisterChildNode('code', TXMLFx_code_type);
  RegisterChildNode('newparam', TXMLProfile_gles2_type_newparam);
  RegisterChildNode('technique', TXMLProfile_gles2_type_technique);
  RegisterChildNode('extra', TXMLExtra_type);
  FInclude := CreateCollection(TXMLFx_include_typeList, IXMLFx_include_type, 'include') as IXMLFx_include_typeList;
  FCode := CreateCollection(TXMLFx_code_typeList, IXMLFx_code_type, 'code') as IXMLFx_code_typeList;
  FNewparam := CreateCollection(TXMLProfile_gles2_type_newparamList, IXMLProfile_gles2_type_newparam, 'newparam') as IXMLProfile_gles2_type_newparamList;
  FTechnique := CreateCollection(TXMLProfile_gles2_type_techniqueList, IXMLProfile_gles2_type_technique, 'technique') as IXMLProfile_gles2_type_techniqueList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_gles2_type.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_gles2_type.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_gles2_type.Get_Language: UnicodeString;
begin
  Result := AttributeNodes['language'].Text;
end;

procedure TXMLProfile_gles2_type.Set_Language(Value: UnicodeString);
begin
  SetAttribute('language', Value);
end;

function TXMLProfile_gles2_type.Get_Platforms: UnicodeString;
begin
  Result := AttributeNodes['platforms'].Text;
end;

procedure TXMLProfile_gles2_type.Set_Platforms(Value: UnicodeString);
begin
  SetAttribute('platforms', Value);
end;

function TXMLProfile_gles2_type.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_gles2_type.Get_Include: IXMLFx_include_typeList;
begin
  Result := FInclude;
end;

function TXMLProfile_gles2_type.Get_Code: IXMLFx_code_typeList;
begin
  Result := FCode;
end;

function TXMLProfile_gles2_type.Get_Newparam: IXMLProfile_gles2_type_newparamList;
begin
  Result := FNewparam;
end;

function TXMLProfile_gles2_type.Get_Technique: IXMLProfile_gles2_type_techniqueList;
begin
  Result := FTechnique;
end;

function TXMLProfile_gles2_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLFx_include_type }

function TXMLFx_include_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLFx_include_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLFx_include_type.Get_Url: UnicodeString;
begin
  Result := AttributeNodes['url'].Text;
end;

procedure TXMLFx_include_type.Set_Url(Value: UnicodeString);
begin
  SetAttribute('url', Value);
end;

{ TXMLFx_include_typeList }

function TXMLFx_include_typeList.Add: IXMLFx_include_type;
begin
  Result := AddItem(-1) as IXMLFx_include_type;
end;

function TXMLFx_include_typeList.Insert(const Index: Integer): IXMLFx_include_type;
begin
  Result := AddItem(Index) as IXMLFx_include_type;
end;

function TXMLFx_include_typeList.Get_Item(Index: Integer): IXMLFx_include_type;
begin
  Result := List[Index] as IXMLFx_include_type;
end;

{ TXMLFx_code_type }

function TXMLFx_code_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLFx_code_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

{ TXMLFx_code_typeList }

function TXMLFx_code_typeList.Add: IXMLFx_code_type;
begin
  Result := AddItem(-1) as IXMLFx_code_type;
end;

function TXMLFx_code_typeList.Insert(const Index: Integer): IXMLFx_code_type;
begin
  Result := AddItem(Index) as IXMLFx_code_type;
end;

function TXMLFx_code_typeList.Get_Item(Index: Integer): IXMLFx_code_type;
begin
  Result := List[Index] as IXMLFx_code_type;
end;

{ TXMLGles2_newparam_type }

procedure TXMLGles2_newparam_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('usertype', TXMLUsertype);
  RegisterChildNode('array', TXMLArray_);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  inherited;
end;

function TXMLGles2_newparam_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLGles2_newparam_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLGles2_newparam_type.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Modifier: UnicodeString;
begin
  Result := ChildNodes['modifier'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Modifier(Value: UnicodeString);
begin
  ChildNodes['modifier'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLGles2_newparam_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Bvec2: UnicodeString;
begin
  Result := ChildNodes['bvec2'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Bvec2(Value: UnicodeString);
begin
  ChildNodes['bvec2'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Bvec3: UnicodeString;
begin
  Result := ChildNodes['bvec3'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Bvec3(Value: UnicodeString);
begin
  ChildNodes['bvec3'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Bvec4: UnicodeString;
begin
  Result := ChildNodes['bvec4'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Bvec4(Value: UnicodeString);
begin
  ChildNodes['bvec4'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLGles2_newparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Vec2: UnicodeString;
begin
  Result := ChildNodes['vec2'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Vec2(Value: UnicodeString);
begin
  ChildNodes['vec2'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Vec3: UnicodeString;
begin
  Result := ChildNodes['vec3'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Vec3(Value: UnicodeString);
begin
  ChildNodes['vec3'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Vec4: UnicodeString;
begin
  Result := ChildNodes['vec4'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Vec4(Value: UnicodeString);
begin
  ChildNodes['vec4'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Mat2: UnicodeString;
begin
  Result := ChildNodes['mat2'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Mat2(Value: UnicodeString);
begin
  ChildNodes['mat2'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Mat3: UnicodeString;
begin
  Result := ChildNodes['mat3'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Mat3(Value: UnicodeString);
begin
  ChildNodes['mat3'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Mat4: UnicodeString;
begin
  Result := ChildNodes['mat4'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Mat4(Value: UnicodeString);
begin
  ChildNodes['mat4'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLGles2_newparam_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Ivec2: UnicodeString;
begin
  Result := ChildNodes['ivec2'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Ivec2(Value: UnicodeString);
begin
  ChildNodes['ivec2'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Ivec3: UnicodeString;
begin
  Result := ChildNodes['ivec3'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Ivec3(Value: UnicodeString);
begin
  ChildNodes['ivec3'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Ivec4: UnicodeString;
begin
  Result := ChildNodes['ivec4'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Ivec4(Value: UnicodeString);
begin
  ChildNodes['ivec4'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLGles2_newparam_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLGles2_newparam_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLGles2_newparam_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLGles2_newparam_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLGles2_newparam_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLGles2_newparam_type.Get_Usertype: IXMLUsertype;
begin
  Result := ChildNodes['usertype'] as IXMLUsertype;
end;

function TXMLGles2_newparam_type.Get_Array_: IXMLArray_;
begin
  Result := ChildNodes['array'] as IXMLArray_;
end;

{ TXMLProfile_gles2_type_newparamList }

function TXMLProfile_gles2_type_newparamList.Add: IXMLProfile_gles2_type_newparam;
begin
  Result := AddItem(-1) as IXMLProfile_gles2_type_newparam;
end;

function TXMLProfile_gles2_type_newparamList.Insert(const Index: Integer): IXMLProfile_gles2_type_newparam;
begin
  Result := AddItem(Index) as IXMLProfile_gles2_type_newparam;
end;

function TXMLProfile_gles2_type_newparamList.Get_Item(Index: Integer): IXMLProfile_gles2_type_newparam;
begin
  Result := List[Index] as IXMLProfile_gles2_type_newparam;
end;

{ TXMLProfile_gles2_type_technique }

procedure TXMLProfile_gles2_type_technique.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('pass', TXMLGles2_pass_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FPass := CreateCollection(TXMLGles2_pass_typeList, IXMLGles2_pass_type, 'pass') as IXMLGles2_pass_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_gles2_type_technique.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_gles2_type_technique.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_gles2_type_technique.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_gles2_type_technique.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_gles2_type_technique.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_gles2_type_technique.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLProfile_gles2_type_technique.Get_Pass: IXMLGles2_pass_typeList;
begin
  Result := FPass;
end;

function TXMLProfile_gles2_type_technique.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_gles2_type_techniqueList }

function TXMLProfile_gles2_type_techniqueList.Add: IXMLProfile_gles2_type_technique;
begin
  Result := AddItem(-1) as IXMLProfile_gles2_type_technique;
end;

function TXMLProfile_gles2_type_techniqueList.Insert(const Index: Integer): IXMLProfile_gles2_type_technique;
begin
  Result := AddItem(Index) as IXMLProfile_gles2_type_technique;
end;

function TXMLProfile_gles2_type_techniqueList.Get_Item(Index: Integer): IXMLProfile_gles2_type_technique;
begin
  Result := List[Index] as IXMLProfile_gles2_type_technique;
end;

{ TXMLGles2_pass_type }

procedure TXMLGles2_pass_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('states', TXMLGles2_pass_type_states);
  RegisterChildNode('program', TXMLGles2_program_type);
  RegisterChildNode('evaluate', TXMLGles2_pass_type_evaluate);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLGles2_pass_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLGles2_pass_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLGles2_pass_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLGles2_pass_type.Get_States: IXMLGles2_pass_type_states;
begin
  Result := ChildNodes['states'] as IXMLGles2_pass_type_states;
end;

function TXMLGles2_pass_type.Get_Program_: IXMLGles2_program_type;
begin
  Result := ChildNodes['program'] as IXMLGles2_program_type;
end;

function TXMLGles2_pass_type.Get_Evaluate: IXMLGles2_pass_type_evaluate;
begin
  Result := ChildNodes['evaluate'] as IXMLGles2_pass_type_evaluate;
end;

function TXMLGles2_pass_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGles2_pass_typeList }

function TXMLGles2_pass_typeList.Add: IXMLGles2_pass_type;
begin
  Result := AddItem(-1) as IXMLGles2_pass_type;
end;

function TXMLGles2_pass_typeList.Insert(const Index: Integer): IXMLGles2_pass_type;
begin
  Result := AddItem(Index) as IXMLGles2_pass_type;
end;

function TXMLGles2_pass_typeList.Get_Item(Index: Integer): IXMLGles2_pass_type;
begin
  Result := List[Index] as IXMLGles2_pass_type;
end;

{ TXMLGles2_pass_type_states }

procedure TXMLGles2_pass_type_states.AfterConstruction;
begin
  RegisterChildNode('blend_color', TXMLBlend_color);
  RegisterChildNode('blend_equation', TXMLBlend_equation);
  RegisterChildNode('blend_equation_separate', TXMLBlend_equation_separate);
  RegisterChildNode('blend_func', TXMLBlend_func);
  RegisterChildNode('blend_func_separate', TXMLBlend_func_separate);
  RegisterChildNode('color_mask', TXMLColor_mask);
  RegisterChildNode('cull_face', TXMLCull_face);
  RegisterChildNode('depth_func', TXMLDepth_func);
  RegisterChildNode('depth_mask', TXMLDepth_mask);
  RegisterChildNode('depth_range', TXMLDepth_range);
  RegisterChildNode('front_face', TXMLFront_face);
  RegisterChildNode('line_width', TXMLLine_width);
  RegisterChildNode('polygon_offset', TXMLPolygon_offset);
  RegisterChildNode('point_size', TXMLPoint_size);
  RegisterChildNode('sample_coverage', TXMLSample_coverage);
  RegisterChildNode('scissor', TXMLScissor);
  RegisterChildNode('stencil_func', TXMLStencil_func);
  RegisterChildNode('stencil_func_separate', TXMLStencil_func_separate);
  RegisterChildNode('stencil_mask', TXMLStencil_mask);
  RegisterChildNode('stencil_mask_separate', TXMLStencil_mask_separate);
  RegisterChildNode('stencil_op', TXMLStencil_op);
  RegisterChildNode('stencil_op_separate', TXMLStencil_op_separate);
  RegisterChildNode('blend_enable', TXMLBlend_enable);
  RegisterChildNode('cull_face_enable', TXMLCull_face_enable);
  RegisterChildNode('depth_test_enable', TXMLDepth_test_enable);
  RegisterChildNode('dither_enable', TXMLDither_enable);
  RegisterChildNode('polygon_offset_fill_enable', TXMLPolygon_offset_fill_enable);
  RegisterChildNode('point_size_enable', TXMLPoint_size_enable);
  RegisterChildNode('sample_alpha_to_coverage_enable', TXMLSample_alpha_to_coverage_enable);
  RegisterChildNode('sample_coverage_enable', TXMLSample_coverage_enable);
  RegisterChildNode('scissor_test_enable', TXMLScissor_test_enable);
  RegisterChildNode('stencil_test_enable', TXMLStencil_test_enable);
  inherited;
end;

function TXMLGles2_pass_type_states.Get_Blend_color: IXMLBlend_color;
begin
  Result := ChildNodes['blend_color'] as IXMLBlend_color;
end;

function TXMLGles2_pass_type_states.Get_Blend_equation: IXMLBlend_equation;
begin
  Result := ChildNodes['blend_equation'] as IXMLBlend_equation;
end;

function TXMLGles2_pass_type_states.Get_Blend_equation_separate: IXMLBlend_equation_separate;
begin
  Result := ChildNodes['blend_equation_separate'] as IXMLBlend_equation_separate;
end;

function TXMLGles2_pass_type_states.Get_Blend_func: IXMLBlend_func;
begin
  Result := ChildNodes['blend_func'] as IXMLBlend_func;
end;

function TXMLGles2_pass_type_states.Get_Blend_func_separate: IXMLBlend_func_separate;
begin
  Result := ChildNodes['blend_func_separate'] as IXMLBlend_func_separate;
end;

function TXMLGles2_pass_type_states.Get_Color_mask: IXMLColor_mask;
begin
  Result := ChildNodes['color_mask'] as IXMLColor_mask;
end;

function TXMLGles2_pass_type_states.Get_Cull_face: IXMLCull_face;
begin
  Result := ChildNodes['cull_face'] as IXMLCull_face;
end;

function TXMLGles2_pass_type_states.Get_Depth_func: IXMLDepth_func;
begin
  Result := ChildNodes['depth_func'] as IXMLDepth_func;
end;

function TXMLGles2_pass_type_states.Get_Depth_mask: IXMLDepth_mask;
begin
  Result := ChildNodes['depth_mask'] as IXMLDepth_mask;
end;

function TXMLGles2_pass_type_states.Get_Depth_range: IXMLDepth_range;
begin
  Result := ChildNodes['depth_range'] as IXMLDepth_range;
end;

function TXMLGles2_pass_type_states.Get_Front_face: IXMLFront_face;
begin
  Result := ChildNodes['front_face'] as IXMLFront_face;
end;

function TXMLGles2_pass_type_states.Get_Line_width: IXMLLine_width;
begin
  Result := ChildNodes['line_width'] as IXMLLine_width;
end;

function TXMLGles2_pass_type_states.Get_Polygon_offset: IXMLPolygon_offset;
begin
  Result := ChildNodes['polygon_offset'] as IXMLPolygon_offset;
end;

function TXMLGles2_pass_type_states.Get_Point_size: IXMLPoint_size;
begin
  Result := ChildNodes['point_size'] as IXMLPoint_size;
end;

function TXMLGles2_pass_type_states.Get_Sample_coverage: IXMLSample_coverage;
begin
  Result := ChildNodes['sample_coverage'] as IXMLSample_coverage;
end;

function TXMLGles2_pass_type_states.Get_Scissor: IXMLScissor;
begin
  Result := ChildNodes['scissor'] as IXMLScissor;
end;

function TXMLGles2_pass_type_states.Get_Stencil_func: IXMLStencil_func;
begin
  Result := ChildNodes['stencil_func'] as IXMLStencil_func;
end;

function TXMLGles2_pass_type_states.Get_Stencil_func_separate: IXMLStencil_func_separate;
begin
  Result := ChildNodes['stencil_func_separate'] as IXMLStencil_func_separate;
end;

function TXMLGles2_pass_type_states.Get_Stencil_mask: IXMLStencil_mask;
begin
  Result := ChildNodes['stencil_mask'] as IXMLStencil_mask;
end;

function TXMLGles2_pass_type_states.Get_Stencil_mask_separate: IXMLStencil_mask_separate;
begin
  Result := ChildNodes['stencil_mask_separate'] as IXMLStencil_mask_separate;
end;

function TXMLGles2_pass_type_states.Get_Stencil_op: IXMLStencil_op;
begin
  Result := ChildNodes['stencil_op'] as IXMLStencil_op;
end;

function TXMLGles2_pass_type_states.Get_Stencil_op_separate: IXMLStencil_op_separate;
begin
  Result := ChildNodes['stencil_op_separate'] as IXMLStencil_op_separate;
end;

function TXMLGles2_pass_type_states.Get_Blend_enable: IXMLBlend_enable;
begin
  Result := ChildNodes['blend_enable'] as IXMLBlend_enable;
end;

function TXMLGles2_pass_type_states.Get_Cull_face_enable: IXMLCull_face_enable;
begin
  Result := ChildNodes['cull_face_enable'] as IXMLCull_face_enable;
end;

function TXMLGles2_pass_type_states.Get_Depth_test_enable: IXMLDepth_test_enable;
begin
  Result := ChildNodes['depth_test_enable'] as IXMLDepth_test_enable;
end;

function TXMLGles2_pass_type_states.Get_Dither_enable: IXMLDither_enable;
begin
  Result := ChildNodes['dither_enable'] as IXMLDither_enable;
end;

function TXMLGles2_pass_type_states.Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
begin
  Result := ChildNodes['polygon_offset_fill_enable'] as IXMLPolygon_offset_fill_enable;
end;

function TXMLGles2_pass_type_states.Get_Point_size_enable: IXMLPoint_size_enable;
begin
  Result := ChildNodes['point_size_enable'] as IXMLPoint_size_enable;
end;

function TXMLGles2_pass_type_states.Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
begin
  Result := ChildNodes['sample_alpha_to_coverage_enable'] as IXMLSample_alpha_to_coverage_enable;
end;

function TXMLGles2_pass_type_states.Get_Sample_coverage_enable: IXMLSample_coverage_enable;
begin
  Result := ChildNodes['sample_coverage_enable'] as IXMLSample_coverage_enable;
end;

function TXMLGles2_pass_type_states.Get_Scissor_test_enable: IXMLScissor_test_enable;
begin
  Result := ChildNodes['scissor_test_enable'] as IXMLScissor_test_enable;
end;

function TXMLGles2_pass_type_states.Get_Stencil_test_enable: IXMLStencil_test_enable;
begin
  Result := ChildNodes['stencil_test_enable'] as IXMLStencil_test_enable;
end;


{ TXMLFx_annotate_type }

function TXMLFx_annotate_type.Get_Name: UnicodeString;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLFx_annotate_type.Set_Name(Value: UnicodeString);
begin
  SetAttribute('name', Value);
end;

function TXMLFx_annotate_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLFx_annotate_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLFx_annotate_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLFx_annotate_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLFx_annotate_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLFx_annotate_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLFx_annotate_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLFx_annotate_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLFx_annotate_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLFx_annotate_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLFx_annotate_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLFx_annotate_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLFx_annotate_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLFx_annotate_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLFx_annotate_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLFx_annotate_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLFx_annotate_type.Get_String_: UnicodeString;
begin
  Result := ChildNodes['string'].Text;
end;

procedure TXMLFx_annotate_type.Set_String_(Value: UnicodeString);
begin
  ChildNodes['string'].NodeValue := Value;
end;

{ TXMLFx_annotate_typeList }

function TXMLFx_annotate_typeList.Add: IXMLFx_annotate_type;
begin
  Result := AddItem(-1) as IXMLFx_annotate_type;
end;

function TXMLFx_annotate_typeList.Insert(const Index: Integer): IXMLFx_annotate_type;
begin
  Result := AddItem(Index) as IXMLFx_annotate_type;
end;

function TXMLFx_annotate_typeList.Get_Item(Index: Integer): IXMLFx_annotate_type;
begin
  Result := List[Index] as IXMLFx_annotate_type;
end;

{ TXMLFx_newparam_type }

procedure TXMLFx_newparam_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  inherited;
end;

function TXMLFx_newparam_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLFx_newparam_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLFx_newparam_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLFx_newparam_type.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLFx_newparam_type.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Modifier: UnicodeString;
begin
  Result := ChildNodes['modifier'].Text;
end;

procedure TXMLFx_newparam_type.Set_Modifier(Value: UnicodeString);
begin
  ChildNodes['modifier'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLFx_newparam_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLFx_newparam_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLFx_newparam_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLFx_newparam_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLFx_newparam_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLFx_newparam_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLFx_newparam_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLFx_newparam_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLFx_newparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float2x1: UnicodeString;
begin
  Result := ChildNodes['float2x1'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float2x1(Value: UnicodeString);
begin
  ChildNodes['float2x1'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float2x3: UnicodeString;
begin
  Result := ChildNodes['float2x3'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float2x3(Value: UnicodeString);
begin
  ChildNodes['float2x3'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float2x4: UnicodeString;
begin
  Result := ChildNodes['float2x4'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float2x4(Value: UnicodeString);
begin
  ChildNodes['float2x4'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float3x1: UnicodeString;
begin
  Result := ChildNodes['float3x1'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float3x1(Value: UnicodeString);
begin
  ChildNodes['float3x1'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float3x2: UnicodeString;
begin
  Result := ChildNodes['float3x2'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float3x2(Value: UnicodeString);
begin
  ChildNodes['float3x2'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float3x4: UnicodeString;
begin
  Result := ChildNodes['float3x4'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float3x4(Value: UnicodeString);
begin
  ChildNodes['float3x4'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float4x1: UnicodeString;
begin
  Result := ChildNodes['float4x1'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float4x1(Value: UnicodeString);
begin
  ChildNodes['float4x1'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float4x2: UnicodeString;
begin
  Result := ChildNodes['float4x2'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float4x2(Value: UnicodeString);
begin
  ChildNodes['float4x2'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float4x3: UnicodeString;
begin
  Result := ChildNodes['float4x3'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float4x3(Value: UnicodeString);
begin
  ChildNodes['float4x3'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLFx_newparam_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLFx_newparam_type.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLFx_newparam_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLFx_newparam_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLFx_newparam_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLFx_newparam_type.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLFx_newparam_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLFx_newparam_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLFx_newparam_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

{ TXMLFx_newparam_typeList }

function TXMLFx_newparam_typeList.Add: IXMLFx_newparam_type;
begin
  Result := AddItem(-1) as IXMLFx_newparam_type;
end;

function TXMLFx_newparam_typeList.Insert(const Index: Integer): IXMLFx_newparam_type;
begin
  Result := AddItem(Index) as IXMLFx_newparam_type;
end;

function TXMLFx_newparam_typeList.Get_Item(Index: Integer): IXMLFx_newparam_type;
begin
  Result := List[Index] as IXMLFx_newparam_type;
end;

{ TXMLFx_sampler_type }

procedure TXMLFx_sampler_type.AfterConstruction;
begin
  RegisterChildNode('instance_image', TXMLInstance_image_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLFx_sampler_type.Get_Instance_image: IXMLInstance_image_type;
begin
  Result := ChildNodes['instance_image'] as IXMLInstance_image_type;
end;

function TXMLFx_sampler_type.Get_Wrap_s: UnicodeString;
begin
  Result := ChildNodes['wrap_s'].Text;
end;

procedure TXMLFx_sampler_type.Set_Wrap_s(Value: UnicodeString);
begin
  ChildNodes['wrap_s'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Wrap_t: UnicodeString;
begin
  Result := ChildNodes['wrap_t'].Text;
end;

procedure TXMLFx_sampler_type.Set_Wrap_t(Value: UnicodeString);
begin
  ChildNodes['wrap_t'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Wrap_p: UnicodeString;
begin
  Result := ChildNodes['wrap_p'].Text;
end;

procedure TXMLFx_sampler_type.Set_Wrap_p(Value: UnicodeString);
begin
  ChildNodes['wrap_p'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Minfilter: UnicodeString;
begin
  Result := ChildNodes['minfilter'].Text;
end;

procedure TXMLFx_sampler_type.Set_Minfilter(Value: UnicodeString);
begin
  ChildNodes['minfilter'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Magfilter: UnicodeString;
begin
  Result := ChildNodes['magfilter'].Text;
end;

procedure TXMLFx_sampler_type.Set_Magfilter(Value: UnicodeString);
begin
  ChildNodes['magfilter'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Mipfilter: UnicodeString;
begin
  Result := ChildNodes['mipfilter'].Text;
end;

procedure TXMLFx_sampler_type.Set_Mipfilter(Value: UnicodeString);
begin
  ChildNodes['mipfilter'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Border_color: UnicodeString;
begin
  Result := ChildNodes['border_color'].Text;
end;

procedure TXMLFx_sampler_type.Set_Border_color(Value: UnicodeString);
begin
  ChildNodes['border_color'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Mip_max_level: Byte;
begin
  Result := ChildNodes['mip_max_level'].NodeValue;
end;

procedure TXMLFx_sampler_type.Set_Mip_max_level(Value: Byte);
begin
  ChildNodes['mip_max_level'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Mip_min_level: Byte;
begin
  Result := ChildNodes['mip_min_level'].NodeValue;
end;

procedure TXMLFx_sampler_type.Set_Mip_min_level(Value: Byte);
begin
  ChildNodes['mip_min_level'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Mip_bias: Single;
begin
  Result := ChildNodes['mip_bias'].NodeValue;
end;

procedure TXMLFx_sampler_type.Set_Mip_bias(Value: Single);
begin
  ChildNodes['mip_bias'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Max_anisotropy: LongWord;
begin
  Result := ChildNodes['max_anisotropy'].NodeValue;
end;

procedure TXMLFx_sampler_type.Set_Max_anisotropy(Value: LongWord);
begin
  ChildNodes['max_anisotropy'].NodeValue := Value;
end;

function TXMLFx_sampler_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLInstance_image_type }

{ TXMLFx_sampler1D_type }

{ TXMLFx_sampler2D_type }

{ TXMLFx_sampler3D_type }

{ TXMLFx_samplerCUBE_type }

{ TXMLFx_samplerRECT_type }

{ TXMLFx_samplerDEPTH_type }
{ TXMLUsertype }

procedure TXMLUsertype.AfterConstruction;
begin
  RegisterChildNode('setparam', TXMLUsertype_setparam);
  ItemTag := 'setparam';
  ItemInterface := IXMLUsertype_setparam;
  inherited;
end;

function TXMLUsertype.Get_Typename: UnicodeString;
begin
  Result := AttributeNodes['typename'].Text;
end;

procedure TXMLUsertype.Set_Typename(Value: UnicodeString);
begin
  SetAttribute('typename', Value);
end;

function TXMLUsertype.Get_Setparam(Index: Integer): IXMLUsertype_setparam;
begin
  Result := List[Index] as IXMLUsertype_setparam;
end;

function TXMLUsertype.Add: IXMLUsertype_setparam;
begin
  Result := AddItem(-1) as IXMLUsertype_setparam;
end;

function TXMLUsertype.Insert(const Index: Integer): IXMLUsertype_setparam;
begin
  Result := AddItem(Index) as IXMLUsertype_setparam;
end;

{ TXMLUsertype_setparam }

procedure TXMLUsertype_setparam.AfterConstruction;
begin
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('usertype', TXMLUsertype);
  RegisterChildNode('array', TXMLArray_);
  inherited;
end;

function TXMLUsertype_setparam.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLUsertype_setparam.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

function TXMLUsertype_setparam.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLUsertype_setparam.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Bvec2: UnicodeString;
begin
  Result := ChildNodes['bvec2'].Text;
end;

procedure TXMLUsertype_setparam.Set_Bvec2(Value: UnicodeString);
begin
  ChildNodes['bvec2'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Bvec3: UnicodeString;
begin
  Result := ChildNodes['bvec3'].Text;
end;

procedure TXMLUsertype_setparam.Set_Bvec3(Value: UnicodeString);
begin
  ChildNodes['bvec3'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Bvec4: UnicodeString;
begin
  Result := ChildNodes['bvec4'].Text;
end;

procedure TXMLUsertype_setparam.Set_Bvec4(Value: UnicodeString);
begin
  ChildNodes['bvec4'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLUsertype_setparam.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Vec2: UnicodeString;
begin
  Result := ChildNodes['vec2'].Text;
end;

procedure TXMLUsertype_setparam.Set_Vec2(Value: UnicodeString);
begin
  ChildNodes['vec2'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Vec3: UnicodeString;
begin
  Result := ChildNodes['vec3'].Text;
end;

procedure TXMLUsertype_setparam.Set_Vec3(Value: UnicodeString);
begin
  ChildNodes['vec3'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Vec4: UnicodeString;
begin
  Result := ChildNodes['vec4'].Text;
end;

procedure TXMLUsertype_setparam.Set_Vec4(Value: UnicodeString);
begin
  ChildNodes['vec4'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Mat2: UnicodeString;
begin
  Result := ChildNodes['mat2'].Text;
end;

procedure TXMLUsertype_setparam.Set_Mat2(Value: UnicodeString);
begin
  ChildNodes['mat2'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Mat3: UnicodeString;
begin
  Result := ChildNodes['mat3'].Text;
end;

procedure TXMLUsertype_setparam.Set_Mat3(Value: UnicodeString);
begin
  ChildNodes['mat3'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Mat4: UnicodeString;
begin
  Result := ChildNodes['mat4'].Text;
end;

procedure TXMLUsertype_setparam.Set_Mat4(Value: UnicodeString);
begin
  ChildNodes['mat4'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLUsertype_setparam.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Ivec2: UnicodeString;
begin
  Result := ChildNodes['ivec2'].Text;
end;

procedure TXMLUsertype_setparam.Set_Ivec2(Value: UnicodeString);
begin
  ChildNodes['ivec2'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Ivec3: UnicodeString;
begin
  Result := ChildNodes['ivec3'].Text;
end;

procedure TXMLUsertype_setparam.Set_Ivec3(Value: UnicodeString);
begin
  ChildNodes['ivec3'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Ivec4: UnicodeString;
begin
  Result := ChildNodes['ivec4'].Text;
end;

procedure TXMLUsertype_setparam.Set_Ivec4(Value: UnicodeString);
begin
  ChildNodes['ivec4'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLUsertype_setparam.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLUsertype_setparam.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLUsertype_setparam.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLUsertype_setparam.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLUsertype_setparam.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLUsertype_setparam.Get_Usertype: IXMLUsertype;
begin
  Result := ChildNodes['usertype'] as IXMLUsertype;
end;

function TXMLUsertype_setparam.Get_Array_: IXMLArray_;
begin
  Result := ChildNodes['array'] as IXMLArray_;
end;

{ TXMLArray_ }

procedure TXMLArray_.AfterConstruction;
begin
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('usertype', TXMLUsertype);
  RegisterChildNode('array', TXMLArray_);
  inherited;
end;

function TXMLArray_.Get_Length: LongWord;
begin
  Result := AttributeNodes['length'].NodeValue;
end;

procedure TXMLArray_.Set_Length(Value: LongWord);
begin
  SetAttribute('length', Value);
end;

function TXMLArray_.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLArray_.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLArray_.Get_Bvec2: UnicodeString;
begin
  Result := ChildNodes['bvec2'].Text;
end;

procedure TXMLArray_.Set_Bvec2(Value: UnicodeString);
begin
  ChildNodes['bvec2'].NodeValue := Value;
end;

function TXMLArray_.Get_Bvec3: UnicodeString;
begin
  Result := ChildNodes['bvec3'].Text;
end;

procedure TXMLArray_.Set_Bvec3(Value: UnicodeString);
begin
  ChildNodes['bvec3'].NodeValue := Value;
end;

function TXMLArray_.Get_Bvec4: UnicodeString;
begin
  Result := ChildNodes['bvec4'].Text;
end;

procedure TXMLArray_.Set_Bvec4(Value: UnicodeString);
begin
  ChildNodes['bvec4'].NodeValue := Value;
end;

function TXMLArray_.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLArray_.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLArray_.Get_Vec2: UnicodeString;
begin
  Result := ChildNodes['vec2'].Text;
end;

procedure TXMLArray_.Set_Vec2(Value: UnicodeString);
begin
  ChildNodes['vec2'].NodeValue := Value;
end;

function TXMLArray_.Get_Vec3: UnicodeString;
begin
  Result := ChildNodes['vec3'].Text;
end;

procedure TXMLArray_.Set_Vec3(Value: UnicodeString);
begin
  ChildNodes['vec3'].NodeValue := Value;
end;

function TXMLArray_.Get_Vec4: UnicodeString;
begin
  Result := ChildNodes['vec4'].Text;
end;

procedure TXMLArray_.Set_Vec4(Value: UnicodeString);
begin
  ChildNodes['vec4'].NodeValue := Value;
end;

function TXMLArray_.Get_Mat2: UnicodeString;
begin
  Result := ChildNodes['mat2'].Text;
end;

procedure TXMLArray_.Set_Mat2(Value: UnicodeString);
begin
  ChildNodes['mat2'].NodeValue := Value;
end;

function TXMLArray_.Get_Mat3: UnicodeString;
begin
  Result := ChildNodes['mat3'].Text;
end;

procedure TXMLArray_.Set_Mat3(Value: UnicodeString);
begin
  ChildNodes['mat3'].NodeValue := Value;
end;

function TXMLArray_.Get_Mat4: UnicodeString;
begin
  Result := ChildNodes['mat4'].Text;
end;

procedure TXMLArray_.Set_Mat4(Value: UnicodeString);
begin
  ChildNodes['mat4'].NodeValue := Value;
end;

function TXMLArray_.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLArray_.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLArray_.Get_Ivec2: UnicodeString;
begin
  Result := ChildNodes['ivec2'].Text;
end;

procedure TXMLArray_.Set_Ivec2(Value: UnicodeString);
begin
  ChildNodes['ivec2'].NodeValue := Value;
end;

function TXMLArray_.Get_Ivec3: UnicodeString;
begin
  Result := ChildNodes['ivec3'].Text;
end;

procedure TXMLArray_.Set_Ivec3(Value: UnicodeString);
begin
  ChildNodes['ivec3'].NodeValue := Value;
end;

function TXMLArray_.Get_Ivec4: UnicodeString;
begin
  Result := ChildNodes['ivec4'].Text;
end;

procedure TXMLArray_.Set_Ivec4(Value: UnicodeString);
begin
  ChildNodes['ivec4'].NodeValue := Value;
end;

function TXMLArray_.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLArray_.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLArray_.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLArray_.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLArray_.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLArray_.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLArray_.Get_Usertype: IXMLUsertype;
begin
  Result := ChildNodes['usertype'] as IXMLUsertype;
end;

function TXMLArray_.Get_Array_: IXMLArray_;
begin
  Result := ChildNodes['array'] as IXMLArray_;
end;


{ TXMLBlend_color }

function TXMLBlend_color.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_color.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_color.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_color.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_equation }

function TXMLBlend_equation.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_equation.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_equation.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_equation.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_equation_separate }

procedure TXMLBlend_equation_separate.AfterConstruction;
begin
  RegisterChildNode('rgb', TXMLBlend_equation_separate_rgb);
  RegisterChildNode('alpha', TXMLBlend_equation_separate_alpha);
  inherited;
end;

function TXMLBlend_equation_separate.Get_Rgb: IXMLBlend_equation_separate_rgb;
begin
  Result := ChildNodes['rgb'] as IXMLBlend_equation_separate_rgb;
end;

function TXMLBlend_equation_separate.Get_Alpha: IXMLBlend_equation_separate_alpha;
begin
  Result := ChildNodes['alpha'] as IXMLBlend_equation_separate_alpha;
end;

{ TXMLBlend_equation_separate_rgb }

function TXMLBlend_equation_separate_rgb.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_equation_separate_rgb.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_equation_separate_rgb.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_equation_separate_rgb.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_equation_separate_alpha }

function TXMLBlend_equation_separate_alpha.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_equation_separate_alpha.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_equation_separate_alpha.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_equation_separate_alpha.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_func }

procedure TXMLBlend_func.AfterConstruction;
begin
  RegisterChildNode('src', TXMLBlend_func_src);
  RegisterChildNode('dest', TXMLBlend_func_dest);
  inherited;
end;

function TXMLBlend_func.Get_Src: IXMLBlend_func_src;
begin
  Result := ChildNodes['src'] as IXMLBlend_func_src;
end;

function TXMLBlend_func.Get_Dest: IXMLBlend_func_dest;
begin
  Result := ChildNodes['dest'] as IXMLBlend_func_dest;
end;

{ TXMLBlend_func_src }

function TXMLBlend_func_src.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_func_src.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_func_src.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_func_src.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_func_dest }

function TXMLBlend_func_dest.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_func_dest.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_func_dest.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_func_dest.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_func_separate }

procedure TXMLBlend_func_separate.AfterConstruction;
begin
  RegisterChildNode('src_rgb', TXMLBlend_func_separate_src_rgb);
  RegisterChildNode('dest_rgb', TXMLBlend_func_separate_dest_rgb);
  RegisterChildNode('src_alpha', TXMLBlend_func_separate_src_alpha);
  RegisterChildNode('dest_alpha', TXMLBlend_func_separate_dest_alpha);
  inherited;
end;

function TXMLBlend_func_separate.Get_Src_rgb: IXMLBlend_func_separate_src_rgb;
begin
  Result := ChildNodes['src_rgb'] as IXMLBlend_func_separate_src_rgb;
end;

function TXMLBlend_func_separate.Get_Dest_rgb: IXMLBlend_func_separate_dest_rgb;
begin
  Result := ChildNodes['dest_rgb'] as IXMLBlend_func_separate_dest_rgb;
end;

function TXMLBlend_func_separate.Get_Src_alpha: IXMLBlend_func_separate_src_alpha;
begin
  Result := ChildNodes['src_alpha'] as IXMLBlend_func_separate_src_alpha;
end;

function TXMLBlend_func_separate.Get_Dest_alpha: IXMLBlend_func_separate_dest_alpha;
begin
  Result := ChildNodes['dest_alpha'] as IXMLBlend_func_separate_dest_alpha;
end;

{ TXMLBlend_func_separate_src_rgb }

function TXMLBlend_func_separate_src_rgb.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_func_separate_src_rgb.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_func_separate_src_rgb.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_func_separate_src_rgb.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_func_separate_dest_rgb }

function TXMLBlend_func_separate_dest_rgb.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_func_separate_dest_rgb.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_func_separate_dest_rgb.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_func_separate_dest_rgb.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_func_separate_src_alpha }

function TXMLBlend_func_separate_src_alpha.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_func_separate_src_alpha.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_func_separate_src_alpha.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_func_separate_src_alpha.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_func_separate_dest_alpha }

function TXMLBlend_func_separate_dest_alpha.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLBlend_func_separate_dest_alpha.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_func_separate_dest_alpha.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_func_separate_dest_alpha.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLColor_mask }

function TXMLColor_mask.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLColor_mask.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLColor_mask.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLColor_mask.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLCull_face }

function TXMLCull_face.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLCull_face.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLCull_face.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLCull_face.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDepth_func }

function TXMLDepth_func.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLDepth_func.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_func.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_func.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDepth_mask }

function TXMLDepth_mask.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLDepth_mask.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_mask.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_mask.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDepth_range }

function TXMLDepth_range.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLDepth_range.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_range.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_range.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFront_face }

function TXMLFront_face.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLFront_face.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLFront_face.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFront_face.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLine_width }

function TXMLLine_width.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLine_width.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLLine_width.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLine_width.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_offset }

function TXMLPolygon_offset.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLPolygon_offset.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_offset.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_offset.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_size }

function TXMLPoint_size.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPoint_size.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_size.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_size.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLSample_coverage }

procedure TXMLSample_coverage.AfterConstruction;
begin
  RegisterChildNode('value', TXMLSample_coverage_value);
  RegisterChildNode('invert', TXMLSample_coverage_invert);
  inherited;
end;

function TXMLSample_coverage.Get_Value: IXMLSample_coverage_value;
begin
  Result := ChildNodes['value'] as IXMLSample_coverage_value;
end;

function TXMLSample_coverage.Get_Invert: IXMLSample_coverage_invert;
begin
  Result := ChildNodes['invert'] as IXMLSample_coverage_invert;
end;

{ TXMLSample_coverage_value }

function TXMLSample_coverage_value.Get_Value: Single;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLSample_coverage_value.Set_Value(Value: Single);
begin
  SetAttribute('value', Value);
end;

function TXMLSample_coverage_value.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLSample_coverage_value.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLSample_coverage_invert }

function TXMLSample_coverage_invert.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLSample_coverage_invert.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLSample_coverage_invert.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLSample_coverage_invert.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLScissor }

function TXMLScissor.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLScissor.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLScissor.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLScissor.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func }

procedure TXMLStencil_func.AfterConstruction;
begin
  RegisterChildNode('func', TXMLStencil_func_func);
  RegisterChildNode('ref', TXMLStencil_func_ref);
  RegisterChildNode('mask', TXMLStencil_func_mask);
  inherited;
end;

function TXMLStencil_func.Get_Func: IXMLStencil_func_func;
begin
  Result := ChildNodes['func'] as IXMLStencil_func_func;
end;

function TXMLStencil_func.Get_Ref: IXMLStencil_func_ref;
begin
  Result := ChildNodes['ref'] as IXMLStencil_func_ref;
end;

function TXMLStencil_func.Get_Mask: IXMLStencil_func_mask;
begin
  Result := ChildNodes['mask'] as IXMLStencil_func_mask;
end;

{ TXMLStencil_func_func }

function TXMLStencil_func_func.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_func_func.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_func.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_func.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func_ref }

function TXMLStencil_func_ref.Get_Value: Byte;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_func_ref.Set_Value(Value: Byte);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_ref.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_ref.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func_mask }

function TXMLStencil_func_mask.Get_Value: Byte;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_func_mask.Set_Value(Value: Byte);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_mask.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_mask.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func_separate }

procedure TXMLStencil_func_separate.AfterConstruction;
begin
  RegisterChildNode('front', TXMLStencil_func_separate_front);
  RegisterChildNode('back', TXMLStencil_func_separate_back);
  RegisterChildNode('ref', TXMLStencil_func_separate_ref);
  RegisterChildNode('mask', TXMLStencil_func_separate_mask);
  inherited;
end;

function TXMLStencil_func_separate.Get_Front: IXMLStencil_func_separate_front;
begin
  Result := ChildNodes['front'] as IXMLStencil_func_separate_front;
end;

function TXMLStencil_func_separate.Get_Back: IXMLStencil_func_separate_back;
begin
  Result := ChildNodes['back'] as IXMLStencil_func_separate_back;
end;

function TXMLStencil_func_separate.Get_Ref: IXMLStencil_func_separate_ref;
begin
  Result := ChildNodes['ref'] as IXMLStencil_func_separate_ref;
end;

function TXMLStencil_func_separate.Get_Mask: IXMLStencil_func_separate_mask;
begin
  Result := ChildNodes['mask'] as IXMLStencil_func_separate_mask;
end;

{ TXMLStencil_func_separate_front }

function TXMLStencil_func_separate_front.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_func_separate_front.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_separate_front.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_separate_front.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func_separate_back }

function TXMLStencil_func_separate_back.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_func_separate_back.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_separate_back.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_separate_back.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func_separate_ref }

function TXMLStencil_func_separate_ref.Get_Value: Byte;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_func_separate_ref.Set_Value(Value: Byte);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_separate_ref.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_separate_ref.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_func_separate_mask }

function TXMLStencil_func_separate_mask.Get_Value: Byte;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_func_separate_mask.Set_Value(Value: Byte);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_func_separate_mask.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_func_separate_mask.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_mask }

function TXMLStencil_mask.Get_Value: Int64;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_mask.Set_Value(Value: Int64);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_mask.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_mask.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_mask_separate }

procedure TXMLStencil_mask_separate.AfterConstruction;
begin
  RegisterChildNode('face', TXMLStencil_mask_separate_face);
  RegisterChildNode('mask', TXMLStencil_mask_separate_mask);
  inherited;
end;

function TXMLStencil_mask_separate.Get_Face: IXMLStencil_mask_separate_face;
begin
  Result := ChildNodes['face'] as IXMLStencil_mask_separate_face;
end;

function TXMLStencil_mask_separate.Get_Mask: IXMLStencil_mask_separate_mask;
begin
  Result := ChildNodes['mask'] as IXMLStencil_mask_separate_mask;
end;

{ TXMLStencil_mask_separate_face }

function TXMLStencil_mask_separate_face.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_mask_separate_face.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_mask_separate_face.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_mask_separate_face.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_mask_separate_mask }

function TXMLStencil_mask_separate_mask.Get_Value: Byte;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_mask_separate_mask.Set_Value(Value: Byte);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_mask_separate_mask.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_mask_separate_mask.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op }

procedure TXMLStencil_op.AfterConstruction;
begin
  RegisterChildNode('fail', TXMLStencil_op_fail);
  RegisterChildNode('zfail', TXMLStencil_op_zfail);
  RegisterChildNode('zpass', TXMLStencil_op_zpass);
  inherited;
end;

function TXMLStencil_op.Get_Fail: IXMLStencil_op_fail;
begin
  Result := ChildNodes['fail'] as IXMLStencil_op_fail;
end;

function TXMLStencil_op.Get_Zfail: IXMLStencil_op_zfail;
begin
  Result := ChildNodes['zfail'] as IXMLStencil_op_zfail;
end;

function TXMLStencil_op.Get_Zpass: IXMLStencil_op_zpass;
begin
  Result := ChildNodes['zpass'] as IXMLStencil_op_zpass;
end;

{ TXMLStencil_op_fail }

function TXMLStencil_op_fail.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_fail.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_fail.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_fail.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op_zfail }

function TXMLStencil_op_zfail.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_zfail.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_zfail.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_zfail.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op_zpass }

function TXMLStencil_op_zpass.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_zpass.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_zpass.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_zpass.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op_separate }

procedure TXMLStencil_op_separate.AfterConstruction;
begin
  RegisterChildNode('face', TXMLStencil_op_separate_face);
  RegisterChildNode('fail', TXMLStencil_op_separate_fail);
  RegisterChildNode('zfail', TXMLStencil_op_separate_zfail);
  RegisterChildNode('zpass', TXMLStencil_op_separate_zpass);
  inherited;
end;

function TXMLStencil_op_separate.Get_Face: IXMLStencil_op_separate_face;
begin
  Result := ChildNodes['face'] as IXMLStencil_op_separate_face;
end;

function TXMLStencil_op_separate.Get_Fail: IXMLStencil_op_separate_fail;
begin
  Result := ChildNodes['fail'] as IXMLStencil_op_separate_fail;
end;

function TXMLStencil_op_separate.Get_Zfail: IXMLStencil_op_separate_zfail;
begin
  Result := ChildNodes['zfail'] as IXMLStencil_op_separate_zfail;
end;

function TXMLStencil_op_separate.Get_Zpass: IXMLStencil_op_separate_zpass;
begin
  Result := ChildNodes['zpass'] as IXMLStencil_op_separate_zpass;
end;

{ TXMLStencil_op_separate_face }

function TXMLStencil_op_separate_face.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_separate_face.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_separate_face.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_separate_face.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op_separate_fail }

function TXMLStencil_op_separate_fail.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_separate_fail.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_separate_fail.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_separate_fail.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op_separate_zfail }

function TXMLStencil_op_separate_zfail.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_separate_zfail.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_separate_zfail.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_separate_zfail.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_op_separate_zpass }

function TXMLStencil_op_separate_zpass.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLStencil_op_separate_zpass.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_op_separate_zpass.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_op_separate_zpass.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLBlend_enable }

function TXMLBlend_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLBlend_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLBlend_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLBlend_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLCull_face_enable }

function TXMLCull_face_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLCull_face_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLCull_face_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLCull_face_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDepth_test_enable }

function TXMLDepth_test_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLDepth_test_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_test_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_test_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDither_enable }

function TXMLDither_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLDither_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLDither_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDither_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_offset_fill_enable }

function TXMLPolygon_offset_fill_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPolygon_offset_fill_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_offset_fill_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_offset_fill_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_size_enable }

function TXMLPoint_size_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPoint_size_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_size_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_size_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLSample_alpha_to_coverage_enable }

function TXMLSample_alpha_to_coverage_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLSample_alpha_to_coverage_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLSample_alpha_to_coverage_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLSample_alpha_to_coverage_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLSample_coverage_enable }

function TXMLSample_coverage_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLSample_coverage_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLSample_coverage_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLSample_coverage_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLScissor_test_enable }

function TXMLScissor_test_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLScissor_test_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLScissor_test_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLScissor_test_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLStencil_test_enable }

function TXMLStencil_test_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLStencil_test_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLStencil_test_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLStencil_test_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLGles2_program_type }

procedure TXMLGles2_program_type.AfterConstruction;
begin
  RegisterChildNode('shader', TXMLGles2_shader_type);
  RegisterChildNode('linker', TXMLFx_target_type);
  RegisterChildNode('bind_attribute', TXMLGles2_program_type_bind_attribute);
  RegisterChildNode('bind_uniform', TXMLGles2_program_type_bind_uniform);
  FShader := CreateCollection(TXMLGles2_shader_typeList, IXMLGles2_shader_type, 'shader') as IXMLGles2_shader_typeList;
  FLinker := CreateCollection(TXMLFx_target_typeList, IXMLFx_target_type, 'linker') as IXMLFx_target_typeList;
  FBind_attribute := CreateCollection(TXMLGles2_program_type_bind_attributeList, IXMLGles2_program_type_bind_attribute, 'bind_attribute') as IXMLGles2_program_type_bind_attributeList;
  FBind_uniform := CreateCollection(TXMLGles2_program_type_bind_uniformList, IXMLGles2_program_type_bind_uniform, 'bind_uniform') as IXMLGles2_program_type_bind_uniformList;
  inherited;
end;

function TXMLGles2_program_type.Get_Shader: IXMLGles2_shader_typeList;
begin
  Result := FShader;
end;

function TXMLGles2_program_type.Get_Linker: IXMLFx_target_typeList;
begin
  Result := FLinker;
end;

function TXMLGles2_program_type.Get_Bind_attribute: IXMLGles2_program_type_bind_attributeList;
begin
  Result := FBind_attribute;
end;

function TXMLGles2_program_type.Get_Bind_uniform: IXMLGles2_program_type_bind_uniformList;
begin
  Result := FBind_uniform;
end;

{ TXMLGles2_shader_type }

procedure TXMLGles2_shader_type.AfterConstruction;
begin
  RegisterChildNode('sources', TXMLGles2_shader_type_sources);
  RegisterChildNode('compiler', TXMLFx_target_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FCompiler := CreateCollection(TXMLFx_target_typeList, IXMLFx_target_type, 'compiler') as IXMLFx_target_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLGles2_shader_type.Get_Stage: UnicodeString;
begin
  Result := AttributeNodes['stage'].Text;
end;

procedure TXMLGles2_shader_type.Set_Stage(Value: UnicodeString);
begin
  SetAttribute('stage', Value);
end;

function TXMLGles2_shader_type.Get_Sources: IXMLGles2_shader_type_sources;
begin
  Result := ChildNodes['sources'] as IXMLGles2_shader_type_sources;
end;

function TXMLGles2_shader_type.Get_Compiler: IXMLFx_target_typeList;
begin
  Result := FCompiler;
end;

function TXMLGles2_shader_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGles2_shader_typeList }

function TXMLGles2_shader_typeList.Add: IXMLGles2_shader_type;
begin
  Result := AddItem(-1) as IXMLGles2_shader_type;
end;

function TXMLGles2_shader_typeList.Insert(const Index: Integer): IXMLGles2_shader_type;
begin
  Result := AddItem(Index) as IXMLGles2_shader_type;
end;

function TXMLGles2_shader_typeList.Get_Item(Index: Integer): IXMLGles2_shader_type;
begin
  Result := List[Index] as IXMLGles2_shader_type;
end;

{ TXMLFx_sources_type }

procedure TXMLFx_sources_type.AfterConstruction;
begin
  RegisterChildNode('import', TXMLFx_sources_type_import);
  FInline_ := CreateCollection(TXMLString_List, IXMLNode, 'inline') as IXMLString_List;
  FImport := CreateCollection(TXMLFx_sources_type_importList, IXMLFx_sources_type_import, 'import') as IXMLFx_sources_type_importList;
  inherited;
end;

function TXMLFx_sources_type.Get_Inline_: IXMLString_List;
begin
  Result := FInline_;
end;

function TXMLFx_sources_type.Get_Import: IXMLFx_sources_type_importList;
begin
  Result := FImport;
end;

{ TXMLFx_sources_type_import }

function TXMLFx_sources_type_import.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLFx_sources_type_import.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLFx_sources_type_importList }

function TXMLFx_sources_type_importList.Add: IXMLFx_sources_type_import;
begin
  Result := AddItem(-1) as IXMLFx_sources_type_import;
end;

function TXMLFx_sources_type_importList.Insert(const Index: Integer): IXMLFx_sources_type_import;
begin
  Result := AddItem(Index) as IXMLFx_sources_type_import;
end;

function TXMLFx_sources_type_importList.Get_Item(Index: Integer): IXMLFx_sources_type_import;
begin
  Result := List[Index] as IXMLFx_sources_type_import;
end;

{ TXMLGles2_shader_type_sources }

function TXMLGles2_shader_type_sources.Get_Entry: UnicodeString;
begin
  Result := AttributeNodes['entry'].Text;
end;

procedure TXMLGles2_shader_type_sources.Set_Entry(Value: UnicodeString);
begin
  SetAttribute('entry', Value);
end;

{ TXMLFx_target_type }

procedure TXMLFx_target_type.AfterConstruction;
begin
  RegisterChildNode('binary', TXMLFx_target_type_binary);
  inherited;
end;

function TXMLFx_target_type.Get_Platform: UnicodeString;
begin
  Result := AttributeNodes['platform'].Text;
end;

procedure TXMLFx_target_type.Set_Platform(Value: UnicodeString);
begin
  SetAttribute('platform', Value);
end;

function TXMLFx_target_type.Get_Target: UnicodeString;
begin
  Result := AttributeNodes['target'].Text;
end;

procedure TXMLFx_target_type.Set_Target(Value: UnicodeString);
begin
  SetAttribute('target', Value);
end;

function TXMLFx_target_type.Get_Options: UnicodeString;
begin
  Result := AttributeNodes['options'].Text;
end;

procedure TXMLFx_target_type.Set_Options(Value: UnicodeString);
begin
  SetAttribute('options', Value);
end;

function TXMLFx_target_type.Get_Binary: IXMLFx_target_type_binary;
begin
  Result := ChildNodes['binary'] as IXMLFx_target_type_binary;
end;

{ TXMLFx_target_typeList }

function TXMLFx_target_typeList.Add: IXMLFx_target_type;
begin
  Result := AddItem(-1) as IXMLFx_target_type;
end;

function TXMLFx_target_typeList.Insert(const Index: Integer): IXMLFx_target_type;
begin
  Result := AddItem(Index) as IXMLFx_target_type;
end;

function TXMLFx_target_typeList.Get_Item(Index: Integer): IXMLFx_target_type;
begin
  Result := List[Index] as IXMLFx_target_type;
end;

{ TXMLFx_target_type_binary }

procedure TXMLFx_target_type_binary.AfterConstruction;
begin
  RegisterChildNode('hex', TXMLFx_target_type_binary_hex);
  inherited;
end;

function TXMLFx_target_type_binary.Get_Ref: UnicodeString;
begin
  Result := ChildNodes['ref'].Text;
end;

procedure TXMLFx_target_type_binary.Set_Ref(Value: UnicodeString);
begin
  ChildNodes['ref'].NodeValue := Value;
end;

function TXMLFx_target_type_binary.Get_Hex: IXMLFx_target_type_binary_hex;
begin
  Result := ChildNodes['hex'] as IXMLFx_target_type_binary_hex;
end;

{ TXMLFx_target_type_binary_hex }

function TXMLFx_target_type_binary_hex.Get_Format: UnicodeString;
begin
  Result := AttributeNodes['format'].Text;
end;

procedure TXMLFx_target_type_binary_hex.Set_Format(Value: UnicodeString);
begin
  SetAttribute('format', Value);
end;

{ TXMLGles2_program_type_bind_attribute }

function TXMLGles2_program_type_bind_attribute.Get_Symbol: UnicodeString;
begin
  Result := AttributeNodes['symbol'].Text;
end;

procedure TXMLGles2_program_type_bind_attribute.Set_Symbol(Value: UnicodeString);
begin
  SetAttribute('symbol', Value);
end;

function TXMLGles2_program_type_bind_attribute.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLGles2_program_type_bind_attribute.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

{ TXMLGles2_program_type_bind_attributeList }

function TXMLGles2_program_type_bind_attributeList.Add: IXMLGles2_program_type_bind_attribute;
begin
  Result := AddItem(-1) as IXMLGles2_program_type_bind_attribute;
end;

function TXMLGles2_program_type_bind_attributeList.Insert(const Index: Integer): IXMLGles2_program_type_bind_attribute;
begin
  Result := AddItem(Index) as IXMLGles2_program_type_bind_attribute;
end;

function TXMLGles2_program_type_bind_attributeList.Get_Item(Index: Integer): IXMLGles2_program_type_bind_attribute;
begin
  Result := List[Index] as IXMLGles2_program_type_bind_attribute;
end;

{ TXMLGles2_program_type_bind_uniform }

procedure TXMLGles2_program_type_bind_uniform.AfterConstruction;
begin
  RegisterChildNode('param', TXMLGles2_program_type_bind_uniform_param);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('usertype', TXMLUsertype);
  RegisterChildNode('array', TXMLArray_);
  inherited;
end;

function TXMLGles2_program_type_bind_uniform.Get_Symbol: UnicodeString;
begin
  Result := AttributeNodes['symbol'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Symbol(Value: UnicodeString);
begin
  SetAttribute('symbol', Value);
end;

function TXMLGles2_program_type_bind_uniform.Get_Param: IXMLGles2_program_type_bind_uniform_param;
begin
  Result := ChildNodes['param'] as IXMLGles2_program_type_bind_uniform_param;
end;

function TXMLGles2_program_type_bind_uniform.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Bvec2: UnicodeString;
begin
  Result := ChildNodes['bvec2'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Bvec2(Value: UnicodeString);
begin
  ChildNodes['bvec2'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Bvec3: UnicodeString;
begin
  Result := ChildNodes['bvec3'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Bvec3(Value: UnicodeString);
begin
  ChildNodes['bvec3'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Bvec4: UnicodeString;
begin
  Result := ChildNodes['bvec4'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Bvec4(Value: UnicodeString);
begin
  ChildNodes['bvec4'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Vec2: UnicodeString;
begin
  Result := ChildNodes['vec2'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Vec2(Value: UnicodeString);
begin
  ChildNodes['vec2'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Vec3: UnicodeString;
begin
  Result := ChildNodes['vec3'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Vec3(Value: UnicodeString);
begin
  ChildNodes['vec3'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Vec4: UnicodeString;
begin
  Result := ChildNodes['vec4'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Vec4(Value: UnicodeString);
begin
  ChildNodes['vec4'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Mat2: UnicodeString;
begin
  Result := ChildNodes['mat2'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Mat2(Value: UnicodeString);
begin
  ChildNodes['mat2'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Mat3: UnicodeString;
begin
  Result := ChildNodes['mat3'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Mat3(Value: UnicodeString);
begin
  ChildNodes['mat3'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Mat4: UnicodeString;
begin
  Result := ChildNodes['mat4'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Mat4(Value: UnicodeString);
begin
  ChildNodes['mat4'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Ivec2: UnicodeString;
begin
  Result := ChildNodes['ivec2'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Ivec2(Value: UnicodeString);
begin
  ChildNodes['ivec2'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Ivec3: UnicodeString;
begin
  Result := ChildNodes['ivec3'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Ivec3(Value: UnicodeString);
begin
  ChildNodes['ivec3'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Ivec4: UnicodeString;
begin
  Result := ChildNodes['ivec4'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Ivec4(Value: UnicodeString);
begin
  ChildNodes['ivec4'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLGles2_program_type_bind_uniform.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLGles2_program_type_bind_uniform.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLGles2_program_type_bind_uniform.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLGles2_program_type_bind_uniform.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLGles2_program_type_bind_uniform.Get_Usertype: IXMLUsertype;
begin
  Result := ChildNodes['usertype'] as IXMLUsertype;
end;

function TXMLGles2_program_type_bind_uniform.Get_Array_: IXMLArray_;
begin
  Result := ChildNodes['array'] as IXMLArray_;
end;

{ TXMLGles2_program_type_bind_uniformList }

function TXMLGles2_program_type_bind_uniformList.Add: IXMLGles2_program_type_bind_uniform;
begin
  Result := AddItem(-1) as IXMLGles2_program_type_bind_uniform;
end;

function TXMLGles2_program_type_bind_uniformList.Insert(const Index: Integer): IXMLGles2_program_type_bind_uniform;
begin
  Result := AddItem(Index) as IXMLGles2_program_type_bind_uniform;
end;

function TXMLGles2_program_type_bind_uniformList.Get_Item(Index: Integer): IXMLGles2_program_type_bind_uniform;
begin
  Result := List[Index] as IXMLGles2_program_type_bind_uniform;
end;

{ TXMLGles2_program_type_bind_uniform_param }

function TXMLGles2_program_type_bind_uniform_param.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLGles2_program_type_bind_uniform_param.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLGles2_pass_type_evaluate }

procedure TXMLGles2_pass_type_evaluate.AfterConstruction;
begin
  RegisterChildNode('color_target', TXMLFx_colortarget_type);
  RegisterChildNode('depth_target', TXMLFx_depthtarget_type);
  RegisterChildNode('stencil_target', TXMLFx_stenciltarget_type);
  RegisterChildNode('color_clear', TXMLFx_clearcolor_type);
  RegisterChildNode('stencil_clear', TXMLFx_clearstencil_type);
  RegisterChildNode('depth_clear', TXMLFx_cleardepth_type);
  FColor_target := CreateCollection(TXMLFx_colortarget_typeList, IXMLFx_colortarget_type, 'color_target') as IXMLFx_colortarget_typeList;
  FDepth_target := CreateCollection(TXMLFx_depthtarget_typeList, IXMLFx_depthtarget_type, 'depth_target') as IXMLFx_depthtarget_typeList;
  FStencil_target := CreateCollection(TXMLFx_stenciltarget_typeList, IXMLFx_stenciltarget_type, 'stencil_target') as IXMLFx_stenciltarget_typeList;
  FColor_clear := CreateCollection(TXMLFx_clearcolor_typeList, IXMLFx_clearcolor_type, 'color_clear') as IXMLFx_clearcolor_typeList;
  FStencil_clear := CreateCollection(TXMLFx_clearstencil_typeList, IXMLFx_clearstencil_type, 'stencil_clear') as IXMLFx_clearstencil_typeList;
  FDepth_clear := CreateCollection(TXMLFx_cleardepth_typeList, IXMLFx_cleardepth_type, 'depth_clear') as IXMLFx_cleardepth_typeList;
  inherited;
end;

function TXMLGles2_pass_type_evaluate.Get_Color_target: IXMLFx_colortarget_typeList;
begin
  Result := FColor_target;
end;

function TXMLGles2_pass_type_evaluate.Get_Depth_target: IXMLFx_depthtarget_typeList;
begin
  Result := FDepth_target;
end;

function TXMLGles2_pass_type_evaluate.Get_Stencil_target: IXMLFx_stenciltarget_typeList;
begin
  Result := FStencil_target;
end;

function TXMLGles2_pass_type_evaluate.Get_Color_clear: IXMLFx_clearcolor_typeList;
begin
  Result := FColor_clear;
end;

function TXMLGles2_pass_type_evaluate.Get_Stencil_clear: IXMLFx_clearstencil_typeList;
begin
  Result := FStencil_clear;
end;

function TXMLGles2_pass_type_evaluate.Get_Depth_clear: IXMLFx_cleardepth_typeList;
begin
  Result := FDepth_clear;
end;

function TXMLGles2_pass_type_evaluate.Get_Draw: UnicodeString;
begin
  Result := ChildNodes['draw'].Text;
end;

procedure TXMLGles2_pass_type_evaluate.Set_Draw(Value: UnicodeString);
begin
  ChildNodes['draw'].NodeValue := Value;
end;

{ TXMLFx_rendertarget_type }

procedure TXMLFx_rendertarget_type.AfterConstruction;
begin
  RegisterChildNode('param', TXMLFx_rendertarget_type_param);
  RegisterChildNode('instance_image', TXMLInstance_image_type);
  inherited;
end;

function TXMLFx_rendertarget_type.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLFx_rendertarget_type.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLFx_rendertarget_type.Get_Mip: LongWord;
begin
  Result := AttributeNodes['mip'].NodeValue;
end;

procedure TXMLFx_rendertarget_type.Set_Mip(Value: LongWord);
begin
  SetAttribute('mip', Value);
end;

function TXMLFx_rendertarget_type.Get_Face: UnicodeString;
begin
  Result := AttributeNodes['face'].Text;
end;

procedure TXMLFx_rendertarget_type.Set_Face(Value: UnicodeString);
begin
  SetAttribute('face', Value);
end;

function TXMLFx_rendertarget_type.Get_Slice: LongWord;
begin
  Result := AttributeNodes['slice'].NodeValue;
end;

procedure TXMLFx_rendertarget_type.Set_Slice(Value: LongWord);
begin
  SetAttribute('slice', Value);
end;

function TXMLFx_rendertarget_type.Get_Param: IXMLFx_rendertarget_type_param;
begin
  Result := ChildNodes['param'] as IXMLFx_rendertarget_type_param;
end;

function TXMLFx_rendertarget_type.Get_Instance_image: IXMLInstance_image_type;
begin
  Result := ChildNodes['instance_image'] as IXMLInstance_image_type;
end;

{ TXMLFx_rendertarget_type_param }

function TXMLFx_rendertarget_type_param.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLFx_rendertarget_type_param.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLFx_colortarget_type }

{ TXMLFx_colortarget_typeList }

function TXMLFx_colortarget_typeList.Add: IXMLFx_colortarget_type;
begin
  Result := AddItem(-1) as IXMLFx_colortarget_type;
end;

function TXMLFx_colortarget_typeList.Insert(const Index: Integer): IXMLFx_colortarget_type;
begin
  Result := AddItem(Index) as IXMLFx_colortarget_type;
end;

function TXMLFx_colortarget_typeList.Get_Item(Index: Integer): IXMLFx_colortarget_type;
begin
  Result := List[Index] as IXMLFx_colortarget_type;
end;

{ TXMLFx_depthtarget_type }

{ TXMLFx_depthtarget_typeList }

function TXMLFx_depthtarget_typeList.Add: IXMLFx_depthtarget_type;
begin
  Result := AddItem(-1) as IXMLFx_depthtarget_type;
end;

function TXMLFx_depthtarget_typeList.Insert(const Index: Integer): IXMLFx_depthtarget_type;
begin
  Result := AddItem(Index) as IXMLFx_depthtarget_type;
end;

function TXMLFx_depthtarget_typeList.Get_Item(Index: Integer): IXMLFx_depthtarget_type;
begin
  Result := List[Index] as IXMLFx_depthtarget_type;
end;

{ TXMLFx_stenciltarget_type }

{ TXMLFx_stenciltarget_typeList }

function TXMLFx_stenciltarget_typeList.Add: IXMLFx_stenciltarget_type;
begin
  Result := AddItem(-1) as IXMLFx_stenciltarget_type;
end;

function TXMLFx_stenciltarget_typeList.Insert(const Index: Integer): IXMLFx_stenciltarget_type;
begin
  Result := AddItem(Index) as IXMLFx_stenciltarget_type;
end;

function TXMLFx_stenciltarget_typeList.Get_Item(Index: Integer): IXMLFx_stenciltarget_type;
begin
  Result := List[Index] as IXMLFx_stenciltarget_type;
end;

{ TXMLFx_clearcolor_type }

function TXMLFx_clearcolor_type.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLFx_clearcolor_type.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLFx_clearcolor_typeList }

function TXMLFx_clearcolor_typeList.Add: IXMLFx_clearcolor_type;
begin
  Result := AddItem(-1) as IXMLFx_clearcolor_type;
end;

function TXMLFx_clearcolor_typeList.Insert(const Index: Integer): IXMLFx_clearcolor_type;
begin
  Result := AddItem(Index) as IXMLFx_clearcolor_type;
end;

function TXMLFx_clearcolor_typeList.Get_Item(Index: Integer): IXMLFx_clearcolor_type;
begin
  Result := List[Index] as IXMLFx_clearcolor_type;
end;

{ TXMLFx_clearstencil_type }

function TXMLFx_clearstencil_type.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLFx_clearstencil_type.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLFx_clearstencil_typeList }

function TXMLFx_clearstencil_typeList.Add: IXMLFx_clearstencil_type;
begin
  Result := AddItem(-1) as IXMLFx_clearstencil_type;
end;

function TXMLFx_clearstencil_typeList.Insert(const Index: Integer): IXMLFx_clearstencil_type;
begin
  Result := AddItem(Index) as IXMLFx_clearstencil_type;
end;

function TXMLFx_clearstencil_typeList.Get_Item(Index: Integer): IXMLFx_clearstencil_type;
begin
  Result := List[Index] as IXMLFx_clearstencil_type;
end;

{ TXMLFx_cleardepth_type }

function TXMLFx_cleardepth_type.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLFx_cleardepth_type.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLFx_cleardepth_typeList }

function TXMLFx_cleardepth_typeList.Add: IXMLFx_cleardepth_type;
begin
  Result := AddItem(-1) as IXMLFx_cleardepth_type;
end;

function TXMLFx_cleardepth_typeList.Insert(const Index: Integer): IXMLFx_cleardepth_type;
begin
  Result := AddItem(Index) as IXMLFx_cleardepth_type;
end;

function TXMLFx_cleardepth_typeList.Get_Item(Index: Integer): IXMLFx_cleardepth_type;
begin
  Result := List[Index] as IXMLFx_cleardepth_type;
end;

{ TXMLProfile_glsl_type }

procedure TXMLProfile_glsl_type.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('code', TXMLFx_code_type);
  RegisterChildNode('include', TXMLFx_include_type);
  RegisterChildNode('newparam', TXMLGlsl_newparam_type);
  RegisterChildNode('technique', TXMLProfile_glsl_type_technique);
  RegisterChildNode('extra', TXMLExtra_type);
  FCode := CreateCollection(TXMLFx_code_typeList, IXMLFx_code_type, 'code') as IXMLFx_code_typeList;
  FInclude := CreateCollection(TXMLFx_include_typeList, IXMLFx_include_type, 'include') as IXMLFx_include_typeList;
  FNewparam := CreateCollection(TXMLGlsl_newparam_typeList, IXMLGlsl_newparam_type, 'newparam') as IXMLGlsl_newparam_typeList;
  FTechnique := CreateCollection(TXMLProfile_glsl_type_techniqueList, IXMLProfile_glsl_type_technique, 'technique') as IXMLProfile_glsl_type_techniqueList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_glsl_type.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_glsl_type.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_glsl_type.Get_Platform: UnicodeString;
begin
  Result := AttributeNodes['platform'].Text;
end;

procedure TXMLProfile_glsl_type.Set_Platform(Value: UnicodeString);
begin
  SetAttribute('platform', Value);
end;

function TXMLProfile_glsl_type.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_glsl_type.Get_Code: IXMLFx_code_typeList;
begin
  Result := FCode;
end;

function TXMLProfile_glsl_type.Get_Include: IXMLFx_include_typeList;
begin
  Result := FInclude;
end;

function TXMLProfile_glsl_type.Get_Newparam: IXMLGlsl_newparam_typeList;
begin
  Result := FNewparam;
end;

function TXMLProfile_glsl_type.Get_Technique: IXMLProfile_glsl_type_techniqueList;
begin
  Result := FTechnique;
end;

function TXMLProfile_glsl_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGlsl_newparam_type }

procedure TXMLGlsl_newparam_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLGlsl_array_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  inherited;
end;

function TXMLGlsl_newparam_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLGlsl_newparam_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLGlsl_newparam_type.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Modifier: UnicodeString;
begin
  Result := ChildNodes['modifier'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Modifier(Value: UnicodeString);
begin
  ChildNodes['modifier'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLGlsl_newparam_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLGlsl_newparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLGlsl_newparam_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLGlsl_newparam_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLGlsl_newparam_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLGlsl_newparam_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLGlsl_newparam_type.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLGlsl_newparam_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLGlsl_newparam_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLGlsl_newparam_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLGlsl_newparam_type.Get_Array_: IXMLGlsl_array_type;
begin
  Result := ChildNodes['array'] as IXMLGlsl_array_type;
end;

{ TXMLGlsl_newparam_typeList }

function TXMLGlsl_newparam_typeList.Add: IXMLGlsl_newparam_type;
begin
  Result := AddItem(-1) as IXMLGlsl_newparam_type;
end;

function TXMLGlsl_newparam_typeList.Insert(const Index: Integer): IXMLGlsl_newparam_type;
begin
  Result := AddItem(Index) as IXMLGlsl_newparam_type;
end;

function TXMLGlsl_newparam_typeList.Get_Item(Index: Integer): IXMLGlsl_newparam_type;
begin
  Result := List[Index] as IXMLGlsl_newparam_type;
end;

{ TXMLGlsl_array_type }

procedure TXMLGlsl_array_type.AfterConstruction;
begin
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLGlsl_array_type);
  inherited;
end;

function TXMLGlsl_array_type.Get_Length: LongWord;
begin
  Result := AttributeNodes['length'].NodeValue;
end;

procedure TXMLGlsl_array_type.Set_Length(Value: LongWord);
begin
  SetAttribute('length', Value);
end;

function TXMLGlsl_array_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLGlsl_array_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLGlsl_array_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLGlsl_array_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLGlsl_array_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLGlsl_array_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLGlsl_array_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLGlsl_array_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLGlsl_array_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLGlsl_array_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLGlsl_array_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLGlsl_array_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLGlsl_array_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLGlsl_array_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLGlsl_array_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLGlsl_array_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLGlsl_array_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLGlsl_array_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLGlsl_array_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLGlsl_array_type.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLGlsl_array_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLGlsl_array_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLGlsl_array_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLGlsl_array_type.Get_Array_: IXMLGlsl_array_type;
begin
  Result := ChildNodes['array'] as IXMLGlsl_array_type;
end;

{ TXMLProfile_glsl_type_technique }

procedure TXMLProfile_glsl_type_technique.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('pass', TXMLProfile_glsl_type_technique_pass);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FPass := CreateCollection(TXMLProfile_glsl_type_technique_passList, IXMLProfile_glsl_type_technique_pass, 'pass') as IXMLProfile_glsl_type_technique_passList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_glsl_type_technique.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_glsl_type_technique.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_glsl_type_technique.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_glsl_type_technique.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_glsl_type_technique.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_glsl_type_technique.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLProfile_glsl_type_technique.Get_Pass: IXMLProfile_glsl_type_technique_passList;
begin
  Result := FPass;
end;

function TXMLProfile_glsl_type_technique.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_glsl_type_techniqueList }

function TXMLProfile_glsl_type_techniqueList.Add: IXMLProfile_glsl_type_technique;
begin
  Result := AddItem(-1) as IXMLProfile_glsl_type_technique;
end;

function TXMLProfile_glsl_type_techniqueList.Insert(const Index: Integer): IXMLProfile_glsl_type_technique;
begin
  Result := AddItem(Index) as IXMLProfile_glsl_type_technique;
end;

function TXMLProfile_glsl_type_techniqueList.Get_Item(Index: Integer): IXMLProfile_glsl_type_technique;
begin
  Result := List[Index] as IXMLProfile_glsl_type_technique;
end;

{ TXMLProfile_glsl_type_technique_pass }

procedure TXMLProfile_glsl_type_technique_pass.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('states', TXMLProfile_glsl_type_technique_pass_states);
  RegisterChildNode('program', TXMLGlsl_program_type);
  RegisterChildNode('evaluate', TXMLProfile_glsl_type_technique_pass_evaluate);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_glsl_type_technique_pass.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_glsl_type_technique_pass.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_glsl_type_technique_pass.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLProfile_glsl_type_technique_pass.Get_States: IXMLProfile_glsl_type_technique_pass_states;
begin
  Result := ChildNodes['states'] as IXMLProfile_glsl_type_technique_pass_states;
end;

function TXMLProfile_glsl_type_technique_pass.Get_Program_: IXMLGlsl_program_type;
begin
  Result := ChildNodes['program'] as IXMLGlsl_program_type;
end;

function TXMLProfile_glsl_type_technique_pass.Get_Evaluate: IXMLProfile_glsl_type_technique_pass_evaluate;
begin
  Result := ChildNodes['evaluate'] as IXMLProfile_glsl_type_technique_pass_evaluate;
end;

function TXMLProfile_glsl_type_technique_pass.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_glsl_type_technique_passList }

function TXMLProfile_glsl_type_technique_passList.Add: IXMLProfile_glsl_type_technique_pass;
begin
  Result := AddItem(-1) as IXMLProfile_glsl_type_technique_pass;
end;

function TXMLProfile_glsl_type_technique_passList.Insert(const Index: Integer): IXMLProfile_glsl_type_technique_pass;
begin
  Result := AddItem(Index) as IXMLProfile_glsl_type_technique_pass;
end;

function TXMLProfile_glsl_type_technique_passList.Get_Item(Index: Integer): IXMLProfile_glsl_type_technique_pass;
begin
  Result := List[Index] as IXMLProfile_glsl_type_technique_pass;
end;

{ TXMLProfile_glsl_type_technique_pass_states }

procedure TXMLProfile_glsl_type_technique_pass_states.AfterConstruction;
begin
  RegisterChildNode('alpha_func', TXMLAlpha_func);
  RegisterChildNode('blend_func', TXMLBlend_func);
  RegisterChildNode('blend_func_separate', TXMLBlend_func_separate);
  RegisterChildNode('blend_equation', TXMLBlend_equation);
  RegisterChildNode('blend_equation_separate', TXMLBlend_equation_separate);
  RegisterChildNode('color_material', TXMLColor_material);
  RegisterChildNode('cull_face', TXMLCull_face);
  RegisterChildNode('depth_func', TXMLDepth_func);
  RegisterChildNode('fog_mode', TXMLFog_mode);
  RegisterChildNode('fog_coord_src', TXMLFog_coord_src);
  RegisterChildNode('front_face', TXMLFront_face);
  RegisterChildNode('light_model_color_control', TXMLLight_model_color_control);
  RegisterChildNode('logic_op', TXMLLogic_op);
  RegisterChildNode('polygon_mode', TXMLPolygon_mode);
  RegisterChildNode('shade_model', TXMLShade_model);
  RegisterChildNode('stencil_func', TXMLStencil_func);
  RegisterChildNode('stencil_op', TXMLStencil_op);
  RegisterChildNode('stencil_func_separate', TXMLStencil_func_separate);
  RegisterChildNode('stencil_op_separate', TXMLStencil_op_separate);
  RegisterChildNode('stencil_mask_separate', TXMLStencil_mask_separate);
  RegisterChildNode('light_enable', TXMLLight_enable);
  RegisterChildNode('light_ambient', TXMLLight_ambient);
  RegisterChildNode('light_diffuse', TXMLLight_diffuse);
  RegisterChildNode('light_specular', TXMLLight_specular);
  RegisterChildNode('light_position', TXMLLight_position);
  RegisterChildNode('light_constant_attenuation', TXMLLight_constant_attenuation);
  RegisterChildNode('light_linear_attenuation', TXMLLight_linear_attenuation);
  RegisterChildNode('light_quadratic_attenuation', TXMLLight_quadratic_attenuation);
  RegisterChildNode('light_spot_cutoff', TXMLLight_spot_cutoff);
  RegisterChildNode('light_spot_direction', TXMLLight_spot_direction);
  RegisterChildNode('light_spot_exponent', TXMLLight_spot_exponent);
  RegisterChildNode('texture1D', TXMLTexture1D);
  RegisterChildNode('texture2D', TXMLTexture2D);
  RegisterChildNode('texture3D', TXMLTexture3D);
  RegisterChildNode('textureCUBE', TXMLTextureCUBE);
  RegisterChildNode('textureRECT', TXMLTextureRECT);
  RegisterChildNode('textureDEPTH', TXMLTextureDEPTH);
  RegisterChildNode('texture1D_enable', TXMLTexture1D_enable);
  RegisterChildNode('texture2D_enable', TXMLTexture2D_enable);
  RegisterChildNode('texture3D_enable', TXMLTexture3D_enable);
  RegisterChildNode('textureCUBE_enable', TXMLTextureCUBE_enable);
  RegisterChildNode('textureRECT_enable', TXMLTextureRECT_enable);
  RegisterChildNode('textureDEPTH_enable', TXMLTextureDEPTH_enable);
  RegisterChildNode('texture_env_color', TXMLTexture_env_color);
  RegisterChildNode('texture_env_mode', TXMLTexture_env_mode);
  RegisterChildNode('clip_plane', TXMLClip_plane);
  RegisterChildNode('clip_plane_enable', TXMLClip_plane_enable);
  RegisterChildNode('blend_color', TXMLBlend_color);
  RegisterChildNode('color_mask', TXMLColor_mask);
  RegisterChildNode('depth_bounds', TXMLDepth_bounds);
  RegisterChildNode('depth_mask', TXMLDepth_mask);
  RegisterChildNode('depth_range', TXMLDepth_range);
  RegisterChildNode('fog_density', TXMLFog_density);
  RegisterChildNode('fog_start', TXMLFog_start);
  RegisterChildNode('fog_end', TXMLFog_end);
  RegisterChildNode('fog_color', TXMLFog_color);
  RegisterChildNode('light_model_ambient', TXMLLight_model_ambient);
  RegisterChildNode('lighting_enable', TXMLLighting_enable);
  RegisterChildNode('line_stipple', TXMLLine_stipple);
  RegisterChildNode('line_width', TXMLLine_width);
  RegisterChildNode('material_ambient', TXMLMaterial_ambient);
  RegisterChildNode('material_diffuse', TXMLMaterial_diffuse);
  RegisterChildNode('material_emission', TXMLMaterial_emission);
  RegisterChildNode('material_shininess', TXMLMaterial_shininess);
  RegisterChildNode('material_specular', TXMLMaterial_specular);
  RegisterChildNode('model_view_matrix', TXMLModel_view_matrix);
  RegisterChildNode('point_distance_attenuation', TXMLPoint_distance_attenuation);
  RegisterChildNode('point_fade_threshold_size', TXMLPoint_fade_threshold_size);
  RegisterChildNode('point_size', TXMLPoint_size);
  RegisterChildNode('point_size_min', TXMLPoint_size_min);
  RegisterChildNode('point_size_max', TXMLPoint_size_max);
  RegisterChildNode('polygon_offset', TXMLPolygon_offset);
  RegisterChildNode('projection_matrix', TXMLProjection_matrix);
  RegisterChildNode('scissor', TXMLScissor);
  RegisterChildNode('stencil_mask', TXMLStencil_mask);
  RegisterChildNode('alpha_test_enable', TXMLAlpha_test_enable);
  RegisterChildNode('blend_enable', TXMLBlend_enable);
  RegisterChildNode('color_logic_op_enable', TXMLColor_logic_op_enable);
  RegisterChildNode('color_material_enable', TXMLColor_material_enable);
  RegisterChildNode('cull_face_enable', TXMLCull_face_enable);
  RegisterChildNode('depth_bounds_enable', TXMLDepth_bounds_enable);
  RegisterChildNode('depth_clamp_enable', TXMLDepth_clamp_enable);
  RegisterChildNode('depth_test_enable', TXMLDepth_test_enable);
  RegisterChildNode('dither_enable', TXMLDither_enable);
  RegisterChildNode('fog_enable', TXMLFog_enable);
  RegisterChildNode('light_model_local_viewer_enable', TXMLLight_model_local_viewer_enable);
  RegisterChildNode('light_model_two_side_enable', TXMLLight_model_two_side_enable);
  RegisterChildNode('line_smooth_enable', TXMLLine_smooth_enable);
  RegisterChildNode('line_stipple_enable', TXMLLine_stipple_enable);
  RegisterChildNode('logic_op_enable', TXMLLogic_op_enable);
  RegisterChildNode('multisample_enable', TXMLMultisample_enable);
  RegisterChildNode('normalize_enable', TXMLNormalize_enable);
  RegisterChildNode('point_smooth_enable', TXMLPoint_smooth_enable);
  RegisterChildNode('polygon_offset_fill_enable', TXMLPolygon_offset_fill_enable);
  RegisterChildNode('polygon_offset_line_enable', TXMLPolygon_offset_line_enable);
  RegisterChildNode('polygon_offset_point_enable', TXMLPolygon_offset_point_enable);
  RegisterChildNode('polygon_smooth_enable', TXMLPolygon_smooth_enable);
  RegisterChildNode('polygon_stipple_enable', TXMLPolygon_stipple_enable);
  RegisterChildNode('rescale_normal_enable', TXMLRescale_normal_enable);
  RegisterChildNode('sample_alpha_to_coverage_enable', TXMLSample_alpha_to_coverage_enable);
  RegisterChildNode('sample_alpha_to_one_enable', TXMLSample_alpha_to_one_enable);
  RegisterChildNode('sample_coverage_enable', TXMLSample_coverage_enable);
  RegisterChildNode('scissor_test_enable', TXMLScissor_test_enable);
  RegisterChildNode('stencil_test_enable', TXMLStencil_test_enable);
  inherited;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Alpha_func: IXMLAlpha_func;
begin
  Result := ChildNodes['alpha_func'] as IXMLAlpha_func;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Blend_func: IXMLBlend_func;
begin
  Result := ChildNodes['blend_func'] as IXMLBlend_func;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Blend_func_separate: IXMLBlend_func_separate;
begin
  Result := ChildNodes['blend_func_separate'] as IXMLBlend_func_separate;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Blend_equation: IXMLBlend_equation;
begin
  Result := ChildNodes['blend_equation'] as IXMLBlend_equation;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Blend_equation_separate: IXMLBlend_equation_separate;
begin
  Result := ChildNodes['blend_equation_separate'] as IXMLBlend_equation_separate;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Color_material: IXMLColor_material;
begin
  Result := ChildNodes['color_material'] as IXMLColor_material;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Cull_face: IXMLCull_face;
begin
  Result := ChildNodes['cull_face'] as IXMLCull_face;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_func: IXMLDepth_func;
begin
  Result := ChildNodes['depth_func'] as IXMLDepth_func;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_mode: IXMLFog_mode;
begin
  Result := ChildNodes['fog_mode'] as IXMLFog_mode;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_coord_src: IXMLFog_coord_src;
begin
  Result := ChildNodes['fog_coord_src'] as IXMLFog_coord_src;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Front_face: IXMLFront_face;
begin
  Result := ChildNodes['front_face'] as IXMLFront_face;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_model_color_control: IXMLLight_model_color_control;
begin
  Result := ChildNodes['light_model_color_control'] as IXMLLight_model_color_control;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Logic_op: IXMLLogic_op;
begin
  Result := ChildNodes['logic_op'] as IXMLLogic_op;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_mode: IXMLPolygon_mode;
begin
  Result := ChildNodes['polygon_mode'] as IXMLPolygon_mode;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Shade_model: IXMLShade_model;
begin
  Result := ChildNodes['shade_model'] as IXMLShade_model;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_func: IXMLStencil_func;
begin
  Result := ChildNodes['stencil_func'] as IXMLStencil_func;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_op: IXMLStencil_op;
begin
  Result := ChildNodes['stencil_op'] as IXMLStencil_op;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_func_separate: IXMLStencil_func_separate;
begin
  Result := ChildNodes['stencil_func_separate'] as IXMLStencil_func_separate;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_op_separate: IXMLStencil_op_separate;
begin
  Result := ChildNodes['stencil_op_separate'] as IXMLStencil_op_separate;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_mask_separate: IXMLStencil_mask_separate;
begin
  Result := ChildNodes['stencil_mask_separate'] as IXMLStencil_mask_separate;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_enable: IXMLLight_enable;
begin
  Result := ChildNodes['light_enable'] as IXMLLight_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_ambient: IXMLLight_ambient;
begin
  Result := ChildNodes['light_ambient'] as IXMLLight_ambient;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_diffuse: IXMLLight_diffuse;
begin
  Result := ChildNodes['light_diffuse'] as IXMLLight_diffuse;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_specular: IXMLLight_specular;
begin
  Result := ChildNodes['light_specular'] as IXMLLight_specular;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_position: IXMLLight_position;
begin
  Result := ChildNodes['light_position'] as IXMLLight_position;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_constant_attenuation: IXMLLight_constant_attenuation;
begin
  Result := ChildNodes['light_constant_attenuation'] as IXMLLight_constant_attenuation;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_linear_attenuation: IXMLLight_linear_attenuation;
begin
  Result := ChildNodes['light_linear_attenuation'] as IXMLLight_linear_attenuation;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_quadratic_attenuation: IXMLLight_quadratic_attenuation;
begin
  Result := ChildNodes['light_quadratic_attenuation'] as IXMLLight_quadratic_attenuation;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_spot_cutoff: IXMLLight_spot_cutoff;
begin
  Result := ChildNodes['light_spot_cutoff'] as IXMLLight_spot_cutoff;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_spot_direction: IXMLLight_spot_direction;
begin
  Result := ChildNodes['light_spot_direction'] as IXMLLight_spot_direction;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_spot_exponent: IXMLLight_spot_exponent;
begin
  Result := ChildNodes['light_spot_exponent'] as IXMLLight_spot_exponent;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture1D: IXMLTexture1D;
begin
  Result := ChildNodes['texture1D'] as IXMLTexture1D;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture2D: IXMLTexture2D;
begin
  Result := ChildNodes['texture2D'] as IXMLTexture2D;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture3D: IXMLTexture3D;
begin
  Result := ChildNodes['texture3D'] as IXMLTexture3D;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_TextureCUBE: IXMLTextureCUBE;
begin
  Result := ChildNodes['textureCUBE'] as IXMLTextureCUBE;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_TextureRECT: IXMLTextureRECT;
begin
  Result := ChildNodes['textureRECT'] as IXMLTextureRECT;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_TextureDEPTH: IXMLTextureDEPTH;
begin
  Result := ChildNodes['textureDEPTH'] as IXMLTextureDEPTH;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture1D_enable: IXMLTexture1D_enable;
begin
  Result := ChildNodes['texture1D_enable'] as IXMLTexture1D_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture2D_enable: IXMLTexture2D_enable;
begin
  Result := ChildNodes['texture2D_enable'] as IXMLTexture2D_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture3D_enable: IXMLTexture3D_enable;
begin
  Result := ChildNodes['texture3D_enable'] as IXMLTexture3D_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_TextureCUBE_enable: IXMLTextureCUBE_enable;
begin
  Result := ChildNodes['textureCUBE_enable'] as IXMLTextureCUBE_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_TextureRECT_enable: IXMLTextureRECT_enable;
begin
  Result := ChildNodes['textureRECT_enable'] as IXMLTextureRECT_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_TextureDEPTH_enable: IXMLTextureDEPTH_enable;
begin
  Result := ChildNodes['textureDEPTH_enable'] as IXMLTextureDEPTH_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture_env_color: IXMLTexture_env_color;
begin
  Result := ChildNodes['texture_env_color'] as IXMLTexture_env_color;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Texture_env_mode: IXMLTexture_env_mode;
begin
  Result := ChildNodes['texture_env_mode'] as IXMLTexture_env_mode;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Clip_plane: IXMLClip_plane;
begin
  Result := ChildNodes['clip_plane'] as IXMLClip_plane;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Clip_plane_enable: IXMLClip_plane_enable;
begin
  Result := ChildNodes['clip_plane_enable'] as IXMLClip_plane_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Blend_color: IXMLBlend_color;
begin
  Result := ChildNodes['blend_color'] as IXMLBlend_color;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Color_mask: IXMLColor_mask;
begin
  Result := ChildNodes['color_mask'] as IXMLColor_mask;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_bounds: IXMLDepth_bounds;
begin
  Result := ChildNodes['depth_bounds'] as IXMLDepth_bounds;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_mask: IXMLDepth_mask;
begin
  Result := ChildNodes['depth_mask'] as IXMLDepth_mask;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_range: IXMLDepth_range;
begin
  Result := ChildNodes['depth_range'] as IXMLDepth_range;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_density: IXMLFog_density;
begin
  Result := ChildNodes['fog_density'] as IXMLFog_density;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_start: IXMLFog_start;
begin
  Result := ChildNodes['fog_start'] as IXMLFog_start;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_end: IXMLFog_end;
begin
  Result := ChildNodes['fog_end'] as IXMLFog_end;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_color: IXMLFog_color;
begin
  Result := ChildNodes['fog_color'] as IXMLFog_color;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_model_ambient: IXMLLight_model_ambient;
begin
  Result := ChildNodes['light_model_ambient'] as IXMLLight_model_ambient;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Lighting_enable: IXMLLighting_enable;
begin
  Result := ChildNodes['lighting_enable'] as IXMLLighting_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Line_stipple: IXMLLine_stipple;
begin
  Result := ChildNodes['line_stipple'] as IXMLLine_stipple;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Line_width: IXMLLine_width;
begin
  Result := ChildNodes['line_width'] as IXMLLine_width;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Material_ambient: IXMLMaterial_ambient;
begin
  Result := ChildNodes['material_ambient'] as IXMLMaterial_ambient;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Material_diffuse: IXMLMaterial_diffuse;
begin
  Result := ChildNodes['material_diffuse'] as IXMLMaterial_diffuse;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Material_emission: IXMLMaterial_emission;
begin
  Result := ChildNodes['material_emission'] as IXMLMaterial_emission;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Material_shininess: IXMLMaterial_shininess;
begin
  Result := ChildNodes['material_shininess'] as IXMLMaterial_shininess;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Material_specular: IXMLMaterial_specular;
begin
  Result := ChildNodes['material_specular'] as IXMLMaterial_specular;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Model_view_matrix: IXMLModel_view_matrix;
begin
  Result := ChildNodes['model_view_matrix'] as IXMLModel_view_matrix;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Point_distance_attenuation: IXMLPoint_distance_attenuation;
begin
  Result := ChildNodes['point_distance_attenuation'] as IXMLPoint_distance_attenuation;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Point_fade_threshold_size: IXMLPoint_fade_threshold_size;
begin
  Result := ChildNodes['point_fade_threshold_size'] as IXMLPoint_fade_threshold_size;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Point_size: IXMLPoint_size;
begin
  Result := ChildNodes['point_size'] as IXMLPoint_size;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Point_size_min: IXMLPoint_size_min;
begin
  Result := ChildNodes['point_size_min'] as IXMLPoint_size_min;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Point_size_max: IXMLPoint_size_max;
begin
  Result := ChildNodes['point_size_max'] as IXMLPoint_size_max;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_offset: IXMLPolygon_offset;
begin
  Result := ChildNodes['polygon_offset'] as IXMLPolygon_offset;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Projection_matrix: IXMLProjection_matrix;
begin
  Result := ChildNodes['projection_matrix'] as IXMLProjection_matrix;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Scissor: IXMLScissor;
begin
  Result := ChildNodes['scissor'] as IXMLScissor;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_mask: IXMLStencil_mask;
begin
  Result := ChildNodes['stencil_mask'] as IXMLStencil_mask;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Alpha_test_enable: IXMLAlpha_test_enable;
begin
  Result := ChildNodes['alpha_test_enable'] as IXMLAlpha_test_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Blend_enable: IXMLBlend_enable;
begin
  Result := ChildNodes['blend_enable'] as IXMLBlend_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Color_logic_op_enable: IXMLColor_logic_op_enable;
begin
  Result := ChildNodes['color_logic_op_enable'] as IXMLColor_logic_op_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Color_material_enable: IXMLColor_material_enable;
begin
  Result := ChildNodes['color_material_enable'] as IXMLColor_material_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Cull_face_enable: IXMLCull_face_enable;
begin
  Result := ChildNodes['cull_face_enable'] as IXMLCull_face_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_bounds_enable: IXMLDepth_bounds_enable;
begin
  Result := ChildNodes['depth_bounds_enable'] as IXMLDepth_bounds_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_clamp_enable: IXMLDepth_clamp_enable;
begin
  Result := ChildNodes['depth_clamp_enable'] as IXMLDepth_clamp_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Depth_test_enable: IXMLDepth_test_enable;
begin
  Result := ChildNodes['depth_test_enable'] as IXMLDepth_test_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Dither_enable: IXMLDither_enable;
begin
  Result := ChildNodes['dither_enable'] as IXMLDither_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Fog_enable: IXMLFog_enable;
begin
  Result := ChildNodes['fog_enable'] as IXMLFog_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_model_local_viewer_enable: IXMLLight_model_local_viewer_enable;
begin
  Result := ChildNodes['light_model_local_viewer_enable'] as IXMLLight_model_local_viewer_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Light_model_two_side_enable: IXMLLight_model_two_side_enable;
begin
  Result := ChildNodes['light_model_two_side_enable'] as IXMLLight_model_two_side_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Line_smooth_enable: IXMLLine_smooth_enable;
begin
  Result := ChildNodes['line_smooth_enable'] as IXMLLine_smooth_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Line_stipple_enable: IXMLLine_stipple_enable;
begin
  Result := ChildNodes['line_stipple_enable'] as IXMLLine_stipple_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Logic_op_enable: IXMLLogic_op_enable;
begin
  Result := ChildNodes['logic_op_enable'] as IXMLLogic_op_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Multisample_enable: IXMLMultisample_enable;
begin
  Result := ChildNodes['multisample_enable'] as IXMLMultisample_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Normalize_enable: IXMLNormalize_enable;
begin
  Result := ChildNodes['normalize_enable'] as IXMLNormalize_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Point_smooth_enable: IXMLPoint_smooth_enable;
begin
  Result := ChildNodes['point_smooth_enable'] as IXMLPoint_smooth_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
begin
  Result := ChildNodes['polygon_offset_fill_enable'] as IXMLPolygon_offset_fill_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_offset_line_enable: IXMLPolygon_offset_line_enable;
begin
  Result := ChildNodes['polygon_offset_line_enable'] as IXMLPolygon_offset_line_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_offset_point_enable: IXMLPolygon_offset_point_enable;
begin
  Result := ChildNodes['polygon_offset_point_enable'] as IXMLPolygon_offset_point_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_smooth_enable: IXMLPolygon_smooth_enable;
begin
  Result := ChildNodes['polygon_smooth_enable'] as IXMLPolygon_smooth_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Polygon_stipple_enable: IXMLPolygon_stipple_enable;
begin
  Result := ChildNodes['polygon_stipple_enable'] as IXMLPolygon_stipple_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Rescale_normal_enable: IXMLRescale_normal_enable;
begin
  Result := ChildNodes['rescale_normal_enable'] as IXMLRescale_normal_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
begin
  Result := ChildNodes['sample_alpha_to_coverage_enable'] as IXMLSample_alpha_to_coverage_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Sample_alpha_to_one_enable: IXMLSample_alpha_to_one_enable;
begin
  Result := ChildNodes['sample_alpha_to_one_enable'] as IXMLSample_alpha_to_one_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Sample_coverage_enable: IXMLSample_coverage_enable;
begin
  Result := ChildNodes['sample_coverage_enable'] as IXMLSample_coverage_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Scissor_test_enable: IXMLScissor_test_enable;
begin
  Result := ChildNodes['scissor_test_enable'] as IXMLScissor_test_enable;
end;

function TXMLProfile_glsl_type_technique_pass_states.Get_Stencil_test_enable: IXMLStencil_test_enable;
begin
  Result := ChildNodes['stencil_test_enable'] as IXMLStencil_test_enable;
end;
{ TXMLAlpha_func }

procedure TXMLAlpha_func.AfterConstruction;
begin
  RegisterChildNode('func', TXMLAlpha_func_func);
  RegisterChildNode('value', TXMLAlpha_func_value);
  inherited;
end;

function TXMLAlpha_func.Get_Func: IXMLAlpha_func_func;
begin
  Result := ChildNodes['func'] as IXMLAlpha_func_func;
end;

function TXMLAlpha_func.Get_Value: IXMLAlpha_func_value;
begin
  Result := ChildNodes['value'] as IXMLAlpha_func_value;
end;

{ TXMLAlpha_func_func }

function TXMLAlpha_func_func.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLAlpha_func_func.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLAlpha_func_func.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLAlpha_func_func.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLAlpha_func_value }

function TXMLAlpha_func_value.Get_Value: Single;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLAlpha_func_value.Set_Value(Value: Single);
begin
  SetAttribute('value', Value);
end;

function TXMLAlpha_func_value.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLAlpha_func_value.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLColor_material }

procedure TXMLColor_material.AfterConstruction;
begin
  RegisterChildNode('face', TXMLColor_material_face);
  RegisterChildNode('mode', TXMLColor_material_mode);
  inherited;
end;

function TXMLColor_material.Get_Face: IXMLColor_material_face;
begin
  Result := ChildNodes['face'] as IXMLColor_material_face;
end;

function TXMLColor_material.Get_Mode: IXMLColor_material_mode;
begin
  Result := ChildNodes['mode'] as IXMLColor_material_mode;
end;

{ TXMLColor_material_face }

function TXMLColor_material_face.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLColor_material_face.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLColor_material_face.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLColor_material_face.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLColor_material_mode }

function TXMLColor_material_mode.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLColor_material_mode.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLColor_material_mode.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLColor_material_mode.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_mode }

function TXMLFog_mode.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLFog_mode.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_mode.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_mode.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_coord_src }

function TXMLFog_coord_src.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLFog_coord_src.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_coord_src.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_coord_src.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLight_model_color_control }

function TXMLLight_model_color_control.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_model_color_control.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_model_color_control.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_model_color_control.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLogic_op }

function TXMLLogic_op.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLogic_op.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLogic_op.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLogic_op.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_mode }

procedure TXMLPolygon_mode.AfterConstruction;
begin
  RegisterChildNode('face', TXMLPolygon_mode_face);
  RegisterChildNode('mode', TXMLPolygon_mode_mode);
  inherited;
end;

function TXMLPolygon_mode.Get_Face: IXMLPolygon_mode_face;
begin
  Result := ChildNodes['face'] as IXMLPolygon_mode_face;
end;

function TXMLPolygon_mode.Get_Mode: IXMLPolygon_mode_mode;
begin
  Result := ChildNodes['mode'] as IXMLPolygon_mode_mode;
end;

{ TXMLPolygon_mode_face }

function TXMLPolygon_mode_face.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLPolygon_mode_face.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_mode_face.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_mode_face.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_mode_mode }

function TXMLPolygon_mode_mode.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLPolygon_mode_mode.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_mode_mode.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_mode_mode.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLShade_model }

function TXMLShade_model.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLShade_model.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLShade_model.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLShade_model.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLight_enable }

function TXMLLight_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_ambient }

function TXMLLight_ambient.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_ambient.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_ambient.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_ambient.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_ambient.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_ambient.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_diffuse }

function TXMLLight_diffuse.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_diffuse.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_diffuse.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_diffuse.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_diffuse.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_diffuse.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_specular }

function TXMLLight_specular.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_specular.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_specular.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_specular.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_specular.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_specular.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_position }

function TXMLLight_position.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_position.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_position.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_position.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_position.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_position.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_constant_attenuation }

function TXMLLight_constant_attenuation.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_constant_attenuation.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_constant_attenuation.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_constant_attenuation.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_constant_attenuation.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_constant_attenuation.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_linear_attenuation }

function TXMLLight_linear_attenuation.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_linear_attenuation.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_linear_attenuation.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_linear_attenuation.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_linear_attenuation.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_linear_attenuation.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_quadratic_attenuation }

function TXMLLight_quadratic_attenuation.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_quadratic_attenuation.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_quadratic_attenuation.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_quadratic_attenuation.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_quadratic_attenuation.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_quadratic_attenuation.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_spot_cutoff }

function TXMLLight_spot_cutoff.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_spot_cutoff.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_spot_cutoff.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_spot_cutoff.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_spot_cutoff.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_spot_cutoff.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_spot_direction }

function TXMLLight_spot_direction.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_spot_direction.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_spot_direction.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_spot_direction.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_spot_direction.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_spot_direction.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLLight_spot_exponent }

function TXMLLight_spot_exponent.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_spot_exponent.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_spot_exponent.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_spot_exponent.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLLight_spot_exponent.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLLight_spot_exponent.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTexture1D }

procedure TXMLTexture1D.AfterConstruction;
begin
  RegisterChildNode('value', TXMLFx_sampler1D_type);
  inherited;
end;

function TXMLTexture1D.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture1D.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLTexture1D.Get_Value: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['value'] as IXMLFx_sampler1D_type;
end;

function TXMLTexture1D.Get_Param: UnicodeString;
begin
  Result := ChildNodes['param'].Text;
end;

procedure TXMLTexture1D.Set_Param(Value: UnicodeString);
begin
  ChildNodes['param'].NodeValue := Value;
end;

{ TXMLTexture2D }

procedure TXMLTexture2D.AfterConstruction;
begin
  RegisterChildNode('value', TXMLFx_sampler2D_type);
  inherited;
end;

function TXMLTexture2D.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture2D.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLTexture2D.Get_Value: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['value'] as IXMLFx_sampler2D_type;
end;

function TXMLTexture2D.Get_Param: UnicodeString;
begin
  Result := ChildNodes['param'].Text;
end;

procedure TXMLTexture2D.Set_Param(Value: UnicodeString);
begin
  ChildNodes['param'].NodeValue := Value;
end;

{ TXMLTexture3D }

procedure TXMLTexture3D.AfterConstruction;
begin
  RegisterChildNode('value', TXMLFx_sampler3D_type);
  inherited;
end;

function TXMLTexture3D.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture3D.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLTexture3D.Get_Value: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['value'] as IXMLFx_sampler3D_type;
end;

function TXMLTexture3D.Get_Param: UnicodeString;
begin
  Result := ChildNodes['param'].Text;
end;

procedure TXMLTexture3D.Set_Param(Value: UnicodeString);
begin
  ChildNodes['param'].NodeValue := Value;
end;

{ TXMLTextureCUBE }

procedure TXMLTextureCUBE.AfterConstruction;
begin
  RegisterChildNode('value', TXMLFx_samplerCUBE_type);
  inherited;
end;

function TXMLTextureCUBE.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTextureCUBE.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLTextureCUBE.Get_Value: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['value'] as IXMLFx_samplerCUBE_type;
end;

function TXMLTextureCUBE.Get_Param: UnicodeString;
begin
  Result := ChildNodes['param'].Text;
end;

procedure TXMLTextureCUBE.Set_Param(Value: UnicodeString);
begin
  ChildNodes['param'].NodeValue := Value;
end;

{ TXMLTextureRECT }

procedure TXMLTextureRECT.AfterConstruction;
begin
  RegisterChildNode('value', TXMLFx_samplerRECT_type);
  inherited;
end;

function TXMLTextureRECT.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTextureRECT.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLTextureRECT.Get_Value: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['value'] as IXMLFx_samplerRECT_type;
end;

function TXMLTextureRECT.Get_Param: UnicodeString;
begin
  Result := ChildNodes['param'].Text;
end;

procedure TXMLTextureRECT.Set_Param(Value: UnicodeString);
begin
  ChildNodes['param'].NodeValue := Value;
end;

{ TXMLTextureDEPTH }

procedure TXMLTextureDEPTH.AfterConstruction;
begin
  RegisterChildNode('value', TXMLFx_samplerDEPTH_type);
  inherited;
end;

function TXMLTextureDEPTH.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTextureDEPTH.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

function TXMLTextureDEPTH.Get_Value: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['value'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLTextureDEPTH.Get_Param: UnicodeString;
begin
  Result := ChildNodes['param'].Text;
end;

procedure TXMLTextureDEPTH.Set_Param(Value: UnicodeString);
begin
  ChildNodes['param'].NodeValue := Value;
end;

{ TXMLTexture1D_enable }

function TXMLTexture1D_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLTexture1D_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLTexture1D_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTexture1D_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTexture1D_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture1D_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTexture2D_enable }

function TXMLTexture2D_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLTexture2D_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLTexture2D_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTexture2D_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTexture2D_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture2D_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTexture3D_enable }

function TXMLTexture3D_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLTexture3D_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLTexture3D_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTexture3D_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTexture3D_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture3D_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTextureCUBE_enable }

function TXMLTextureCUBE_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLTextureCUBE_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLTextureCUBE_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTextureCUBE_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTextureCUBE_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTextureCUBE_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTextureRECT_enable }

function TXMLTextureRECT_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLTextureRECT_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLTextureRECT_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTextureRECT_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTextureRECT_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTextureRECT_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTextureDEPTH_enable }

function TXMLTextureDEPTH_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLTextureDEPTH_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLTextureDEPTH_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTextureDEPTH_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTextureDEPTH_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTextureDEPTH_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTexture_env_color }

function TXMLTexture_env_color.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLTexture_env_color.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLTexture_env_color.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTexture_env_color.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTexture_env_color.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture_env_color.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLTexture_env_mode }

function TXMLTexture_env_mode.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLTexture_env_mode.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLTexture_env_mode.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLTexture_env_mode.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLTexture_env_mode.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLTexture_env_mode.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLClip_plane }

function TXMLClip_plane.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLClip_plane.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLClip_plane.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLClip_plane.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLClip_plane.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLClip_plane.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLClip_plane_enable }

function TXMLClip_plane_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLClip_plane_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLClip_plane_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLClip_plane_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

function TXMLClip_plane_enable.Get_Index: LongWord;
begin
  Result := AttributeNodes['index'].NodeValue;
end;

procedure TXMLClip_plane_enable.Set_Index(Value: LongWord);
begin
  SetAttribute('index', Value);
end;

{ TXMLDepth_bounds }

function TXMLDepth_bounds.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLDepth_bounds.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_bounds.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_bounds.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_density }

function TXMLFog_density.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLFog_density.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_density.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_density.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_start }

function TXMLFog_start.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLFog_start.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_start.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_start.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_end }

function TXMLFog_end.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLFog_end.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_end.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_end.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_color }

function TXMLFog_color.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLFog_color.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_color.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_color.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLight_model_ambient }

function TXMLLight_model_ambient.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLight_model_ambient.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_model_ambient.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_model_ambient.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLighting_enable }

function TXMLLighting_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLighting_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLighting_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLighting_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLine_stipple }

function TXMLLine_stipple.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLLine_stipple.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLLine_stipple.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLine_stipple.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLMaterial_ambient }

function TXMLMaterial_ambient.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLMaterial_ambient.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLMaterial_ambient.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLMaterial_ambient.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLMaterial_diffuse }

function TXMLMaterial_diffuse.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLMaterial_diffuse.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLMaterial_diffuse.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLMaterial_diffuse.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLMaterial_emission }

function TXMLMaterial_emission.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLMaterial_emission.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLMaterial_emission.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLMaterial_emission.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLMaterial_shininess }

function TXMLMaterial_shininess.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLMaterial_shininess.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLMaterial_shininess.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLMaterial_shininess.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLMaterial_specular }

function TXMLMaterial_specular.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLMaterial_specular.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLMaterial_specular.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLMaterial_specular.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLModel_view_matrix }

function TXMLModel_view_matrix.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLModel_view_matrix.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLModel_view_matrix.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLModel_view_matrix.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_distance_attenuation }

function TXMLPoint_distance_attenuation.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLPoint_distance_attenuation.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_distance_attenuation.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_distance_attenuation.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_fade_threshold_size }

function TXMLPoint_fade_threshold_size.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPoint_fade_threshold_size.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_fade_threshold_size.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_fade_threshold_size.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_size_min }

function TXMLPoint_size_min.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPoint_size_min.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_size_min.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_size_min.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_size_max }

function TXMLPoint_size_max.Get_Value: Double;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPoint_size_max.Set_Value(Value: Double);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_size_max.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_size_max.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLProjection_matrix }

function TXMLProjection_matrix.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLProjection_matrix.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLProjection_matrix.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLProjection_matrix.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLAlpha_test_enable }

function TXMLAlpha_test_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLAlpha_test_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLAlpha_test_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLAlpha_test_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLColor_logic_op_enable }

function TXMLColor_logic_op_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLColor_logic_op_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLColor_logic_op_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLColor_logic_op_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLColor_material_enable }

function TXMLColor_material_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLColor_material_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLColor_material_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLColor_material_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDepth_bounds_enable }

function TXMLDepth_bounds_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLDepth_bounds_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_bounds_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_bounds_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLDepth_clamp_enable }

function TXMLDepth_clamp_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLDepth_clamp_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLDepth_clamp_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLDepth_clamp_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLFog_enable }

function TXMLFog_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLFog_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLFog_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLFog_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLight_model_local_viewer_enable }

function TXMLLight_model_local_viewer_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_model_local_viewer_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_model_local_viewer_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_model_local_viewer_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLight_model_two_side_enable }

function TXMLLight_model_two_side_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLight_model_two_side_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLight_model_two_side_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLight_model_two_side_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLine_smooth_enable }

function TXMLLine_smooth_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLine_smooth_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLine_smooth_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLine_smooth_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLine_stipple_enable }

function TXMLLine_stipple_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLine_stipple_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLine_stipple_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLine_stipple_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLLogic_op_enable }

function TXMLLogic_op_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLLogic_op_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLLogic_op_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLLogic_op_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLMultisample_enable }

function TXMLMultisample_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLMultisample_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLMultisample_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLMultisample_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLNormalize_enable }

function TXMLNormalize_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLNormalize_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLNormalize_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLNormalize_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPoint_smooth_enable }

function TXMLPoint_smooth_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPoint_smooth_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPoint_smooth_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPoint_smooth_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_offset_line_enable }

function TXMLPolygon_offset_line_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPolygon_offset_line_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_offset_line_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_offset_line_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_offset_point_enable }

function TXMLPolygon_offset_point_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPolygon_offset_point_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_offset_point_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_offset_point_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_smooth_enable }

function TXMLPolygon_smooth_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPolygon_smooth_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_smooth_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_smooth_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLPolygon_stipple_enable }

function TXMLPolygon_stipple_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLPolygon_stipple_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLPolygon_stipple_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLPolygon_stipple_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLRescale_normal_enable }

function TXMLRescale_normal_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLRescale_normal_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLRescale_normal_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLRescale_normal_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLSample_alpha_to_one_enable }

function TXMLSample_alpha_to_one_enable.Get_Value: Boolean;
begin
  Result := AttributeNodes['value'].NodeValue;
end;

procedure TXMLSample_alpha_to_one_enable.Set_Value(Value: Boolean);
begin
  SetAttribute('value', Value);
end;

function TXMLSample_alpha_to_one_enable.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLSample_alpha_to_one_enable.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLGlsl_program_type }

procedure TXMLGlsl_program_type.AfterConstruction;
begin
  RegisterChildNode('shader', TXMLGlsl_shader_type);
  RegisterChildNode('bind_attribute', TXMLGlsl_program_type_bind_attribute);
  RegisterChildNode('bind_uniform', TXMLGlsl_program_type_bind_uniform);
  FShader := CreateCollection(TXMLGlsl_shader_typeList, IXMLGlsl_shader_type, 'shader') as IXMLGlsl_shader_typeList;
  FBind_attribute := CreateCollection(TXMLGlsl_program_type_bind_attributeList, IXMLGlsl_program_type_bind_attribute, 'bind_attribute') as IXMLGlsl_program_type_bind_attributeList;
  FBind_uniform := CreateCollection(TXMLGlsl_program_type_bind_uniformList, IXMLGlsl_program_type_bind_uniform, 'bind_uniform') as IXMLGlsl_program_type_bind_uniformList;
  inherited;
end;

function TXMLGlsl_program_type.Get_Shader: IXMLGlsl_shader_typeList;
begin
  Result := FShader;
end;

function TXMLGlsl_program_type.Get_Bind_attribute: IXMLGlsl_program_type_bind_attributeList;
begin
  Result := FBind_attribute;
end;

function TXMLGlsl_program_type.Get_Bind_uniform: IXMLGlsl_program_type_bind_uniformList;
begin
  Result := FBind_uniform;
end;

{ TXMLGlsl_shader_type }

procedure TXMLGlsl_shader_type.AfterConstruction;
begin
  RegisterChildNode('sources', TXMLFx_sources_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLGlsl_shader_type.Get_Stage: UnicodeString;
begin
  Result := AttributeNodes['stage'].Text;
end;

procedure TXMLGlsl_shader_type.Set_Stage(Value: UnicodeString);
begin
  SetAttribute('stage', Value);
end;

function TXMLGlsl_shader_type.Get_Sources: IXMLFx_sources_type;
begin
  Result := ChildNodes['sources'] as IXMLFx_sources_type;
end;

function TXMLGlsl_shader_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGlsl_shader_typeList }

function TXMLGlsl_shader_typeList.Add: IXMLGlsl_shader_type;
begin
  Result := AddItem(-1) as IXMLGlsl_shader_type;
end;

function TXMLGlsl_shader_typeList.Insert(const Index: Integer): IXMLGlsl_shader_type;
begin
  Result := AddItem(Index) as IXMLGlsl_shader_type;
end;

function TXMLGlsl_shader_typeList.Get_Item(Index: Integer): IXMLGlsl_shader_type;
begin
  Result := List[Index] as IXMLGlsl_shader_type;
end;

{ TXMLGlsl_program_type_bind_attribute }

function TXMLGlsl_program_type_bind_attribute.Get_Symbol: UnicodeString;
begin
  Result := AttributeNodes['symbol'].Text;
end;

procedure TXMLGlsl_program_type_bind_attribute.Set_Symbol(Value: UnicodeString);
begin
  SetAttribute('symbol', Value);
end;

function TXMLGlsl_program_type_bind_attribute.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLGlsl_program_type_bind_attribute.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

{ TXMLGlsl_program_type_bind_attributeList }

function TXMLGlsl_program_type_bind_attributeList.Add: IXMLGlsl_program_type_bind_attribute;
begin
  Result := AddItem(-1) as IXMLGlsl_program_type_bind_attribute;
end;

function TXMLGlsl_program_type_bind_attributeList.Insert(const Index: Integer): IXMLGlsl_program_type_bind_attribute;
begin
  Result := AddItem(Index) as IXMLGlsl_program_type_bind_attribute;
end;

function TXMLGlsl_program_type_bind_attributeList.Get_Item(Index: Integer): IXMLGlsl_program_type_bind_attribute;
begin
  Result := List[Index] as IXMLGlsl_program_type_bind_attribute;
end;

{ TXMLGlsl_program_type_bind_uniform }

procedure TXMLGlsl_program_type_bind_uniform.AfterConstruction;
begin
  RegisterChildNode('param', TXMLGlsl_program_type_bind_uniform_param);
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLGlsl_array_type);
  inherited;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Symbol: UnicodeString;
begin
  Result := AttributeNodes['symbol'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Symbol(Value: UnicodeString);
begin
  SetAttribute('symbol', Value);
end;

function TXMLGlsl_program_type_bind_uniform.Get_Param: IXMLGlsl_program_type_bind_uniform_param;
begin
  Result := ChildNodes['param'] as IXMLGlsl_program_type_bind_uniform_param;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLGlsl_program_type_bind_uniform.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLGlsl_program_type_bind_uniform.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLGlsl_program_type_bind_uniform.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLGlsl_program_type_bind_uniform.Get_Array_: IXMLGlsl_array_type;
begin
  Result := ChildNodes['array'] as IXMLGlsl_array_type;
end;

{ TXMLGlsl_program_type_bind_uniformList }

function TXMLGlsl_program_type_bind_uniformList.Add: IXMLGlsl_program_type_bind_uniform;
begin
  Result := AddItem(-1) as IXMLGlsl_program_type_bind_uniform;
end;

function TXMLGlsl_program_type_bind_uniformList.Insert(const Index: Integer): IXMLGlsl_program_type_bind_uniform;
begin
  Result := AddItem(Index) as IXMLGlsl_program_type_bind_uniform;
end;

function TXMLGlsl_program_type_bind_uniformList.Get_Item(Index: Integer): IXMLGlsl_program_type_bind_uniform;
begin
  Result := List[Index] as IXMLGlsl_program_type_bind_uniform;
end;

{ TXMLGlsl_program_type_bind_uniform_param }

function TXMLGlsl_program_type_bind_uniform_param.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLGlsl_program_type_bind_uniform_param.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLProfile_glsl_type_technique_pass_evaluate }

procedure TXMLProfile_glsl_type_technique_pass_evaluate.AfterConstruction;
begin
  RegisterChildNode('color_target', TXMLFx_colortarget_type);
  RegisterChildNode('depth_target', TXMLFx_depthtarget_type);
  RegisterChildNode('stencil_target', TXMLFx_stenciltarget_type);
  RegisterChildNode('color_clear', TXMLFx_clearcolor_type);
  RegisterChildNode('depth_clear', TXMLFx_cleardepth_type);
  RegisterChildNode('stencil_clear', TXMLFx_clearstencil_type);
  FColor_target := CreateCollection(TXMLFx_colortarget_typeList, IXMLFx_colortarget_type, 'color_target') as IXMLFx_colortarget_typeList;
  FDepth_target := CreateCollection(TXMLFx_depthtarget_typeList, IXMLFx_depthtarget_type, 'depth_target') as IXMLFx_depthtarget_typeList;
  FStencil_target := CreateCollection(TXMLFx_stenciltarget_typeList, IXMLFx_stenciltarget_type, 'stencil_target') as IXMLFx_stenciltarget_typeList;
  FColor_clear := CreateCollection(TXMLFx_clearcolor_typeList, IXMLFx_clearcolor_type, 'color_clear') as IXMLFx_clearcolor_typeList;
  FDepth_clear := CreateCollection(TXMLFx_cleardepth_typeList, IXMLFx_cleardepth_type, 'depth_clear') as IXMLFx_cleardepth_typeList;
  FStencil_clear := CreateCollection(TXMLFx_clearstencil_typeList, IXMLFx_clearstencil_type, 'stencil_clear') as IXMLFx_clearstencil_typeList;
  inherited;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Color_target: IXMLFx_colortarget_typeList;
begin
  Result := FColor_target;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Depth_target: IXMLFx_depthtarget_typeList;
begin
  Result := FDepth_target;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Stencil_target: IXMLFx_stenciltarget_typeList;
begin
  Result := FStencil_target;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Color_clear: IXMLFx_clearcolor_typeList;
begin
  Result := FColor_clear;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Depth_clear: IXMLFx_cleardepth_typeList;
begin
  Result := FDepth_clear;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Stencil_clear: IXMLFx_clearstencil_typeList;
begin
  Result := FStencil_clear;
end;

function TXMLProfile_glsl_type_technique_pass_evaluate.Get_Draw: UnicodeString;
begin
  Result := ChildNodes['draw'].Text;
end;

procedure TXMLProfile_glsl_type_technique_pass_evaluate.Set_Draw(Value: UnicodeString);
begin
  ChildNodes['draw'].NodeValue := Value;
end;
{ TXMLString_List }

function TXMLString_List.Add(const Value: UnicodeString): IXMLNode;
begin
  Result := AddItem(-1);
  Result.NodeValue := Value;
end;

function TXMLString_List.Insert(const Index: Integer; const Value: UnicodeString): IXMLNode;
begin
  Result := AddItem(Index);
  Result.NodeValue := Value;
end;

function TXMLString_List.Get_Item(Index: Integer): UnicodeString;
begin
  Result := List[Index].NodeValue;
end;


{ TXMLFx_common_newparam_type }

procedure TXMLFx_common_newparam_type.AfterConstruction;
begin
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  inherited;
end;

function TXMLFx_common_newparam_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLFx_common_newparam_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLFx_common_newparam_type.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLFx_common_newparam_type.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

function TXMLFx_common_newparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLFx_common_newparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLFx_common_newparam_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLFx_common_newparam_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLFx_common_newparam_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLFx_common_newparam_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLFx_common_newparam_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLFx_common_newparam_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLFx_common_newparam_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

{ TXMLFx_common_newparam_typeList }

function TXMLFx_common_newparam_typeList.Add: IXMLFx_common_newparam_type;
begin
  Result := AddItem(-1) as IXMLFx_common_newparam_type;
end;

function TXMLFx_common_newparam_typeList.Insert(const Index: Integer): IXMLFx_common_newparam_type;
begin
  Result := AddItem(Index) as IXMLFx_common_newparam_type;
end;

function TXMLFx_common_newparam_typeList.Get_Item(Index: Integer): IXMLFx_common_newparam_type;
begin
  Result := List[Index] as IXMLFx_common_newparam_type;
end;

{ TXMLProfile_cg_type }

procedure TXMLProfile_cg_type.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('code', TXMLFx_code_type);
  RegisterChildNode('include', TXMLFx_include_type);
  RegisterChildNode('newparam', TXMLCg_newparam_type);
  RegisterChildNode('technique', TXMLProfile_cg_type_technique);
  RegisterChildNode('extra', TXMLExtra_type);
  FCode := CreateCollection(TXMLFx_code_typeList, IXMLFx_code_type, 'code') as IXMLFx_code_typeList;
  FInclude := CreateCollection(TXMLFx_include_typeList, IXMLFx_include_type, 'include') as IXMLFx_include_typeList;
  FNewparam := CreateCollection(TXMLCg_newparam_typeList, IXMLCg_newparam_type, 'newparam') as IXMLCg_newparam_typeList;
  FTechnique := CreateCollection(TXMLProfile_cg_type_techniqueList, IXMLProfile_cg_type_technique, 'technique') as IXMLProfile_cg_type_techniqueList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_cg_type.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_cg_type.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_cg_type.Get_Platform: UnicodeString;
begin
  Result := AttributeNodes['platform'].Text;
end;

procedure TXMLProfile_cg_type.Set_Platform(Value: UnicodeString);
begin
  SetAttribute('platform', Value);
end;

function TXMLProfile_cg_type.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_cg_type.Get_Code: IXMLFx_code_typeList;
begin
  Result := FCode;
end;

function TXMLProfile_cg_type.Get_Include: IXMLFx_include_typeList;
begin
  Result := FInclude;
end;

function TXMLProfile_cg_type.Get_Newparam: IXMLCg_newparam_typeList;
begin
  Result := FNewparam;
end;

function TXMLProfile_cg_type.Get_Technique: IXMLProfile_cg_type_techniqueList;
begin
  Result := FTechnique;
end;

function TXMLProfile_cg_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLCg_newparam_type }

procedure TXMLCg_newparam_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLCg_array_type);
  RegisterChildNode('usertype', TXMLCg_user_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  inherited;
end;

function TXMLCg_newparam_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLCg_newparam_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLCg_newparam_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLCg_newparam_type.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLCg_newparam_type.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Modifier: UnicodeString;
begin
  Result := ChildNodes['modifier'].Text;
end;

procedure TXMLCg_newparam_type.Set_Modifier(Value: UnicodeString);
begin
  ChildNodes['modifier'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLCg_newparam_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool2x1: UnicodeString;
begin
  Result := ChildNodes['bool2x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool2x1(Value: UnicodeString);
begin
  ChildNodes['bool2x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool2x2: UnicodeString;
begin
  Result := ChildNodes['bool2x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool2x2(Value: UnicodeString);
begin
  ChildNodes['bool2x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool2x3: UnicodeString;
begin
  Result := ChildNodes['bool2x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool2x3(Value: UnicodeString);
begin
  ChildNodes['bool2x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool2x4: UnicodeString;
begin
  Result := ChildNodes['bool2x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool2x4(Value: UnicodeString);
begin
  ChildNodes['bool2x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool3x1: UnicodeString;
begin
  Result := ChildNodes['bool3x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool3x1(Value: UnicodeString);
begin
  ChildNodes['bool3x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool3x2: UnicodeString;
begin
  Result := ChildNodes['bool3x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool3x2(Value: UnicodeString);
begin
  ChildNodes['bool3x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool3x3: UnicodeString;
begin
  Result := ChildNodes['bool3x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool3x3(Value: UnicodeString);
begin
  ChildNodes['bool3x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool3x4: UnicodeString;
begin
  Result := ChildNodes['bool3x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool3x4(Value: UnicodeString);
begin
  ChildNodes['bool3x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool4x1: UnicodeString;
begin
  Result := ChildNodes['bool4x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool4x1(Value: UnicodeString);
begin
  ChildNodes['bool4x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool4x2: UnicodeString;
begin
  Result := ChildNodes['bool4x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool4x2(Value: UnicodeString);
begin
  ChildNodes['bool4x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool4x3: UnicodeString;
begin
  Result := ChildNodes['bool4x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool4x3(Value: UnicodeString);
begin
  ChildNodes['bool4x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Bool4x4: UnicodeString;
begin
  Result := ChildNodes['bool4x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Bool4x4(Value: UnicodeString);
begin
  ChildNodes['bool4x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLCg_newparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float2x1: UnicodeString;
begin
  Result := ChildNodes['float2x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float2x1(Value: UnicodeString);
begin
  ChildNodes['float2x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float2x3: UnicodeString;
begin
  Result := ChildNodes['float2x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float2x3(Value: UnicodeString);
begin
  ChildNodes['float2x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float2x4: UnicodeString;
begin
  Result := ChildNodes['float2x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float2x4(Value: UnicodeString);
begin
  ChildNodes['float2x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float3x1: UnicodeString;
begin
  Result := ChildNodes['float3x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float3x1(Value: UnicodeString);
begin
  ChildNodes['float3x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float3x2: UnicodeString;
begin
  Result := ChildNodes['float3x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float3x2(Value: UnicodeString);
begin
  ChildNodes['float3x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float3x4: UnicodeString;
begin
  Result := ChildNodes['float3x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float3x4(Value: UnicodeString);
begin
  ChildNodes['float3x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float4x1: UnicodeString;
begin
  Result := ChildNodes['float4x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float4x1(Value: UnicodeString);
begin
  ChildNodes['float4x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float4x2: UnicodeString;
begin
  Result := ChildNodes['float4x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float4x2(Value: UnicodeString);
begin
  ChildNodes['float4x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float4x3: UnicodeString;
begin
  Result := ChildNodes['float4x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float4x3(Value: UnicodeString);
begin
  ChildNodes['float4x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLCg_newparam_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int2x1: UnicodeString;
begin
  Result := ChildNodes['int2x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int2x1(Value: UnicodeString);
begin
  ChildNodes['int2x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int2x2: UnicodeString;
begin
  Result := ChildNodes['int2x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int2x2(Value: UnicodeString);
begin
  ChildNodes['int2x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int2x3: UnicodeString;
begin
  Result := ChildNodes['int2x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int2x3(Value: UnicodeString);
begin
  ChildNodes['int2x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int2x4: UnicodeString;
begin
  Result := ChildNodes['int2x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int2x4(Value: UnicodeString);
begin
  ChildNodes['int2x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int3x1: UnicodeString;
begin
  Result := ChildNodes['int3x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int3x1(Value: UnicodeString);
begin
  ChildNodes['int3x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int3x2: UnicodeString;
begin
  Result := ChildNodes['int3x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int3x2(Value: UnicodeString);
begin
  ChildNodes['int3x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int3x3: UnicodeString;
begin
  Result := ChildNodes['int3x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int3x3(Value: UnicodeString);
begin
  ChildNodes['int3x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int3x4: UnicodeString;
begin
  Result := ChildNodes['int3x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int3x4(Value: UnicodeString);
begin
  ChildNodes['int3x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int4x1: UnicodeString;
begin
  Result := ChildNodes['int4x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int4x1(Value: UnicodeString);
begin
  ChildNodes['int4x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int4x2: UnicodeString;
begin
  Result := ChildNodes['int4x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int4x2(Value: UnicodeString);
begin
  ChildNodes['int4x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int4x3: UnicodeString;
begin
  Result := ChildNodes['int4x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int4x3(Value: UnicodeString);
begin
  ChildNodes['int4x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Int4x4: UnicodeString;
begin
  Result := ChildNodes['int4x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Int4x4(Value: UnicodeString);
begin
  ChildNodes['int4x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half: Double;
begin
  Result := ChildNodes['half'].NodeValue;
end;

procedure TXMLCg_newparam_type.Set_Half(Value: Double);
begin
  ChildNodes['half'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half2: UnicodeString;
begin
  Result := ChildNodes['half2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half2(Value: UnicodeString);
begin
  ChildNodes['half2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half3: UnicodeString;
begin
  Result := ChildNodes['half3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half3(Value: UnicodeString);
begin
  ChildNodes['half3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half4: UnicodeString;
begin
  Result := ChildNodes['half4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half4(Value: UnicodeString);
begin
  ChildNodes['half4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half2x1: UnicodeString;
begin
  Result := ChildNodes['half2x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half2x1(Value: UnicodeString);
begin
  ChildNodes['half2x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half2x2: UnicodeString;
begin
  Result := ChildNodes['half2x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half2x2(Value: UnicodeString);
begin
  ChildNodes['half2x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half2x3: UnicodeString;
begin
  Result := ChildNodes['half2x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half2x3(Value: UnicodeString);
begin
  ChildNodes['half2x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half2x4: UnicodeString;
begin
  Result := ChildNodes['half2x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half2x4(Value: UnicodeString);
begin
  ChildNodes['half2x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half3x1: UnicodeString;
begin
  Result := ChildNodes['half3x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half3x1(Value: UnicodeString);
begin
  ChildNodes['half3x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half3x2: UnicodeString;
begin
  Result := ChildNodes['half3x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half3x2(Value: UnicodeString);
begin
  ChildNodes['half3x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half3x3: UnicodeString;
begin
  Result := ChildNodes['half3x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half3x3(Value: UnicodeString);
begin
  ChildNodes['half3x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half3x4: UnicodeString;
begin
  Result := ChildNodes['half3x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half3x4(Value: UnicodeString);
begin
  ChildNodes['half3x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half4x1: UnicodeString;
begin
  Result := ChildNodes['half4x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half4x1(Value: UnicodeString);
begin
  ChildNodes['half4x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half4x2: UnicodeString;
begin
  Result := ChildNodes['half4x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half4x2(Value: UnicodeString);
begin
  ChildNodes['half4x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half4x3: UnicodeString;
begin
  Result := ChildNodes['half4x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half4x3(Value: UnicodeString);
begin
  ChildNodes['half4x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Half4x4: UnicodeString;
begin
  Result := ChildNodes['half4x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Half4x4(Value: UnicodeString);
begin
  ChildNodes['half4x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed: Double;
begin
  Result := ChildNodes['fixed'].NodeValue;
end;

procedure TXMLCg_newparam_type.Set_Fixed(Value: Double);
begin
  ChildNodes['fixed'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed2: UnicodeString;
begin
  Result := ChildNodes['fixed2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed2(Value: UnicodeString);
begin
  ChildNodes['fixed2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed3: UnicodeString;
begin
  Result := ChildNodes['fixed3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed3(Value: UnicodeString);
begin
  ChildNodes['fixed3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed4: UnicodeString;
begin
  Result := ChildNodes['fixed4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed4(Value: UnicodeString);
begin
  ChildNodes['fixed4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed2x1: UnicodeString;
begin
  Result := ChildNodes['fixed2x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed2x1(Value: UnicodeString);
begin
  ChildNodes['fixed2x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed2x2: UnicodeString;
begin
  Result := ChildNodes['fixed2x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed2x2(Value: UnicodeString);
begin
  ChildNodes['fixed2x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed2x3: UnicodeString;
begin
  Result := ChildNodes['fixed2x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed2x3(Value: UnicodeString);
begin
  ChildNodes['fixed2x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed2x4: UnicodeString;
begin
  Result := ChildNodes['fixed2x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed2x4(Value: UnicodeString);
begin
  ChildNodes['fixed2x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed3x1: UnicodeString;
begin
  Result := ChildNodes['fixed3x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed3x1(Value: UnicodeString);
begin
  ChildNodes['fixed3x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed3x2: UnicodeString;
begin
  Result := ChildNodes['fixed3x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed3x2(Value: UnicodeString);
begin
  ChildNodes['fixed3x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed3x3: UnicodeString;
begin
  Result := ChildNodes['fixed3x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed3x3(Value: UnicodeString);
begin
  ChildNodes['fixed3x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed3x4: UnicodeString;
begin
  Result := ChildNodes['fixed3x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed3x4(Value: UnicodeString);
begin
  ChildNodes['fixed3x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed4x1: UnicodeString;
begin
  Result := ChildNodes['fixed4x1'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed4x1(Value: UnicodeString);
begin
  ChildNodes['fixed4x1'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed4x2: UnicodeString;
begin
  Result := ChildNodes['fixed4x2'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed4x2(Value: UnicodeString);
begin
  ChildNodes['fixed4x2'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed4x3: UnicodeString;
begin
  Result := ChildNodes['fixed4x3'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed4x3(Value: UnicodeString);
begin
  ChildNodes['fixed4x3'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Fixed4x4: UnicodeString;
begin
  Result := ChildNodes['fixed4x4'].Text;
end;

procedure TXMLCg_newparam_type.Set_Fixed4x4(Value: UnicodeString);
begin
  ChildNodes['fixed4x4'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLCg_newparam_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLCg_newparam_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLCg_newparam_type.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLCg_newparam_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLCg_newparam_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLCg_newparam_type.Get_String_: UnicodeString;
begin
  Result := ChildNodes['string'].Text;
end;

procedure TXMLCg_newparam_type.Set_String_(Value: UnicodeString);
begin
  ChildNodes['string'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLCg_newparam_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLCg_newparam_type.Get_Array_: IXMLCg_array_type;
begin
  Result := ChildNodes['array'] as IXMLCg_array_type;
end;

function TXMLCg_newparam_type.Get_Usertype: IXMLCg_user_type;
begin
  Result := ChildNodes['usertype'] as IXMLCg_user_type;
end;

{ TXMLCg_newparam_typeList }

function TXMLCg_newparam_typeList.Add: IXMLCg_newparam_type;
begin
  Result := AddItem(-1) as IXMLCg_newparam_type;
end;

function TXMLCg_newparam_typeList.Insert(const Index: Integer): IXMLCg_newparam_type;
begin
  Result := AddItem(Index) as IXMLCg_newparam_type;
end;

function TXMLCg_newparam_typeList.Get_Item(Index: Integer): IXMLCg_newparam_type;
begin
  Result := List[Index] as IXMLCg_newparam_type;
end;

{ TXMLCg_array_type }

procedure TXMLCg_array_type.AfterConstruction;
begin
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLCg_array_type);
  RegisterChildNode('usertype', TXMLCg_user_type);
  inherited;
end;

function TXMLCg_array_type.Get_Length: LongWord;
begin
  Result := AttributeNodes['length'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Length(Value: LongWord);
begin
  SetAttribute('length', Value);
end;

function TXMLCg_array_type.Get_Resizable: Boolean;
begin
  Result := AttributeNodes['resizable'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Resizable(Value: Boolean);
begin
  SetAttribute('resizable', Value);
end;

function TXMLCg_array_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLCg_array_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLCg_array_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLCg_array_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool2x1: UnicodeString;
begin
  Result := ChildNodes['bool2x1'].Text;
end;

procedure TXMLCg_array_type.Set_Bool2x1(Value: UnicodeString);
begin
  ChildNodes['bool2x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool2x2: UnicodeString;
begin
  Result := ChildNodes['bool2x2'].Text;
end;

procedure TXMLCg_array_type.Set_Bool2x2(Value: UnicodeString);
begin
  ChildNodes['bool2x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool2x3: UnicodeString;
begin
  Result := ChildNodes['bool2x3'].Text;
end;

procedure TXMLCg_array_type.Set_Bool2x3(Value: UnicodeString);
begin
  ChildNodes['bool2x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool2x4: UnicodeString;
begin
  Result := ChildNodes['bool2x4'].Text;
end;

procedure TXMLCg_array_type.Set_Bool2x4(Value: UnicodeString);
begin
  ChildNodes['bool2x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool3x1: UnicodeString;
begin
  Result := ChildNodes['bool3x1'].Text;
end;

procedure TXMLCg_array_type.Set_Bool3x1(Value: UnicodeString);
begin
  ChildNodes['bool3x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool3x2: UnicodeString;
begin
  Result := ChildNodes['bool3x2'].Text;
end;

procedure TXMLCg_array_type.Set_Bool3x2(Value: UnicodeString);
begin
  ChildNodes['bool3x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool3x3: UnicodeString;
begin
  Result := ChildNodes['bool3x3'].Text;
end;

procedure TXMLCg_array_type.Set_Bool3x3(Value: UnicodeString);
begin
  ChildNodes['bool3x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool3x4: UnicodeString;
begin
  Result := ChildNodes['bool3x4'].Text;
end;

procedure TXMLCg_array_type.Set_Bool3x4(Value: UnicodeString);
begin
  ChildNodes['bool3x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool4x1: UnicodeString;
begin
  Result := ChildNodes['bool4x1'].Text;
end;

procedure TXMLCg_array_type.Set_Bool4x1(Value: UnicodeString);
begin
  ChildNodes['bool4x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool4x2: UnicodeString;
begin
  Result := ChildNodes['bool4x2'].Text;
end;

procedure TXMLCg_array_type.Set_Bool4x2(Value: UnicodeString);
begin
  ChildNodes['bool4x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool4x3: UnicodeString;
begin
  Result := ChildNodes['bool4x3'].Text;
end;

procedure TXMLCg_array_type.Set_Bool4x3(Value: UnicodeString);
begin
  ChildNodes['bool4x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Bool4x4: UnicodeString;
begin
  Result := ChildNodes['bool4x4'].Text;
end;

procedure TXMLCg_array_type.Set_Bool4x4(Value: UnicodeString);
begin
  ChildNodes['bool4x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLCg_array_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLCg_array_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLCg_array_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float2x1: UnicodeString;
begin
  Result := ChildNodes['float2x1'].Text;
end;

procedure TXMLCg_array_type.Set_Float2x1(Value: UnicodeString);
begin
  ChildNodes['float2x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLCg_array_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float2x3: UnicodeString;
begin
  Result := ChildNodes['float2x3'].Text;
end;

procedure TXMLCg_array_type.Set_Float2x3(Value: UnicodeString);
begin
  ChildNodes['float2x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float2x4: UnicodeString;
begin
  Result := ChildNodes['float2x4'].Text;
end;

procedure TXMLCg_array_type.Set_Float2x4(Value: UnicodeString);
begin
  ChildNodes['float2x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float3x1: UnicodeString;
begin
  Result := ChildNodes['float3x1'].Text;
end;

procedure TXMLCg_array_type.Set_Float3x1(Value: UnicodeString);
begin
  ChildNodes['float3x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float3x2: UnicodeString;
begin
  Result := ChildNodes['float3x2'].Text;
end;

procedure TXMLCg_array_type.Set_Float3x2(Value: UnicodeString);
begin
  ChildNodes['float3x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLCg_array_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float3x4: UnicodeString;
begin
  Result := ChildNodes['float3x4'].Text;
end;

procedure TXMLCg_array_type.Set_Float3x4(Value: UnicodeString);
begin
  ChildNodes['float3x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float4x1: UnicodeString;
begin
  Result := ChildNodes['float4x1'].Text;
end;

procedure TXMLCg_array_type.Set_Float4x1(Value: UnicodeString);
begin
  ChildNodes['float4x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float4x2: UnicodeString;
begin
  Result := ChildNodes['float4x2'].Text;
end;

procedure TXMLCg_array_type.Set_Float4x2(Value: UnicodeString);
begin
  ChildNodes['float4x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float4x3: UnicodeString;
begin
  Result := ChildNodes['float4x3'].Text;
end;

procedure TXMLCg_array_type.Set_Float4x3(Value: UnicodeString);
begin
  ChildNodes['float4x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLCg_array_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLCg_array_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLCg_array_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLCg_array_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int2x1: UnicodeString;
begin
  Result := ChildNodes['int2x1'].Text;
end;

procedure TXMLCg_array_type.Set_Int2x1(Value: UnicodeString);
begin
  ChildNodes['int2x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int2x2: UnicodeString;
begin
  Result := ChildNodes['int2x2'].Text;
end;

procedure TXMLCg_array_type.Set_Int2x2(Value: UnicodeString);
begin
  ChildNodes['int2x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int2x3: UnicodeString;
begin
  Result := ChildNodes['int2x3'].Text;
end;

procedure TXMLCg_array_type.Set_Int2x3(Value: UnicodeString);
begin
  ChildNodes['int2x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int2x4: UnicodeString;
begin
  Result := ChildNodes['int2x4'].Text;
end;

procedure TXMLCg_array_type.Set_Int2x4(Value: UnicodeString);
begin
  ChildNodes['int2x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int3x1: UnicodeString;
begin
  Result := ChildNodes['int3x1'].Text;
end;

procedure TXMLCg_array_type.Set_Int3x1(Value: UnicodeString);
begin
  ChildNodes['int3x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int3x2: UnicodeString;
begin
  Result := ChildNodes['int3x2'].Text;
end;

procedure TXMLCg_array_type.Set_Int3x2(Value: UnicodeString);
begin
  ChildNodes['int3x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int3x3: UnicodeString;
begin
  Result := ChildNodes['int3x3'].Text;
end;

procedure TXMLCg_array_type.Set_Int3x3(Value: UnicodeString);
begin
  ChildNodes['int3x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int3x4: UnicodeString;
begin
  Result := ChildNodes['int3x4'].Text;
end;

procedure TXMLCg_array_type.Set_Int3x4(Value: UnicodeString);
begin
  ChildNodes['int3x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int4x1: UnicodeString;
begin
  Result := ChildNodes['int4x1'].Text;
end;

procedure TXMLCg_array_type.Set_Int4x1(Value: UnicodeString);
begin
  ChildNodes['int4x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int4x2: UnicodeString;
begin
  Result := ChildNodes['int4x2'].Text;
end;

procedure TXMLCg_array_type.Set_Int4x2(Value: UnicodeString);
begin
  ChildNodes['int4x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int4x3: UnicodeString;
begin
  Result := ChildNodes['int4x3'].Text;
end;

procedure TXMLCg_array_type.Set_Int4x3(Value: UnicodeString);
begin
  ChildNodes['int4x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Int4x4: UnicodeString;
begin
  Result := ChildNodes['int4x4'].Text;
end;

procedure TXMLCg_array_type.Set_Int4x4(Value: UnicodeString);
begin
  ChildNodes['int4x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half: Double;
begin
  Result := ChildNodes['half'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Half(Value: Double);
begin
  ChildNodes['half'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half2: UnicodeString;
begin
  Result := ChildNodes['half2'].Text;
end;

procedure TXMLCg_array_type.Set_Half2(Value: UnicodeString);
begin
  ChildNodes['half2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half3: UnicodeString;
begin
  Result := ChildNodes['half3'].Text;
end;

procedure TXMLCg_array_type.Set_Half3(Value: UnicodeString);
begin
  ChildNodes['half3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half4: UnicodeString;
begin
  Result := ChildNodes['half4'].Text;
end;

procedure TXMLCg_array_type.Set_Half4(Value: UnicodeString);
begin
  ChildNodes['half4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half2x1: UnicodeString;
begin
  Result := ChildNodes['half2x1'].Text;
end;

procedure TXMLCg_array_type.Set_Half2x1(Value: UnicodeString);
begin
  ChildNodes['half2x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half2x2: UnicodeString;
begin
  Result := ChildNodes['half2x2'].Text;
end;

procedure TXMLCg_array_type.Set_Half2x2(Value: UnicodeString);
begin
  ChildNodes['half2x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half2x3: UnicodeString;
begin
  Result := ChildNodes['half2x3'].Text;
end;

procedure TXMLCg_array_type.Set_Half2x3(Value: UnicodeString);
begin
  ChildNodes['half2x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half2x4: UnicodeString;
begin
  Result := ChildNodes['half2x4'].Text;
end;

procedure TXMLCg_array_type.Set_Half2x4(Value: UnicodeString);
begin
  ChildNodes['half2x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half3x1: UnicodeString;
begin
  Result := ChildNodes['half3x1'].Text;
end;

procedure TXMLCg_array_type.Set_Half3x1(Value: UnicodeString);
begin
  ChildNodes['half3x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half3x2: UnicodeString;
begin
  Result := ChildNodes['half3x2'].Text;
end;

procedure TXMLCg_array_type.Set_Half3x2(Value: UnicodeString);
begin
  ChildNodes['half3x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half3x3: UnicodeString;
begin
  Result := ChildNodes['half3x3'].Text;
end;

procedure TXMLCg_array_type.Set_Half3x3(Value: UnicodeString);
begin
  ChildNodes['half3x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half3x4: UnicodeString;
begin
  Result := ChildNodes['half3x4'].Text;
end;

procedure TXMLCg_array_type.Set_Half3x4(Value: UnicodeString);
begin
  ChildNodes['half3x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half4x1: UnicodeString;
begin
  Result := ChildNodes['half4x1'].Text;
end;

procedure TXMLCg_array_type.Set_Half4x1(Value: UnicodeString);
begin
  ChildNodes['half4x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half4x2: UnicodeString;
begin
  Result := ChildNodes['half4x2'].Text;
end;

procedure TXMLCg_array_type.Set_Half4x2(Value: UnicodeString);
begin
  ChildNodes['half4x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half4x3: UnicodeString;
begin
  Result := ChildNodes['half4x3'].Text;
end;

procedure TXMLCg_array_type.Set_Half4x3(Value: UnicodeString);
begin
  ChildNodes['half4x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Half4x4: UnicodeString;
begin
  Result := ChildNodes['half4x4'].Text;
end;

procedure TXMLCg_array_type.Set_Half4x4(Value: UnicodeString);
begin
  ChildNodes['half4x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed: Double;
begin
  Result := ChildNodes['fixed'].NodeValue;
end;

procedure TXMLCg_array_type.Set_Fixed(Value: Double);
begin
  ChildNodes['fixed'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed2: UnicodeString;
begin
  Result := ChildNodes['fixed2'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed2(Value: UnicodeString);
begin
  ChildNodes['fixed2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed3: UnicodeString;
begin
  Result := ChildNodes['fixed3'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed3(Value: UnicodeString);
begin
  ChildNodes['fixed3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed4: UnicodeString;
begin
  Result := ChildNodes['fixed4'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed4(Value: UnicodeString);
begin
  ChildNodes['fixed4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed2x1: UnicodeString;
begin
  Result := ChildNodes['fixed2x1'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed2x1(Value: UnicodeString);
begin
  ChildNodes['fixed2x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed2x2: UnicodeString;
begin
  Result := ChildNodes['fixed2x2'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed2x2(Value: UnicodeString);
begin
  ChildNodes['fixed2x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed2x3: UnicodeString;
begin
  Result := ChildNodes['fixed2x3'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed2x3(Value: UnicodeString);
begin
  ChildNodes['fixed2x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed2x4: UnicodeString;
begin
  Result := ChildNodes['fixed2x4'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed2x4(Value: UnicodeString);
begin
  ChildNodes['fixed2x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed3x1: UnicodeString;
begin
  Result := ChildNodes['fixed3x1'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed3x1(Value: UnicodeString);
begin
  ChildNodes['fixed3x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed3x2: UnicodeString;
begin
  Result := ChildNodes['fixed3x2'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed3x2(Value: UnicodeString);
begin
  ChildNodes['fixed3x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed3x3: UnicodeString;
begin
  Result := ChildNodes['fixed3x3'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed3x3(Value: UnicodeString);
begin
  ChildNodes['fixed3x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed3x4: UnicodeString;
begin
  Result := ChildNodes['fixed3x4'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed3x4(Value: UnicodeString);
begin
  ChildNodes['fixed3x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed4x1: UnicodeString;
begin
  Result := ChildNodes['fixed4x1'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed4x1(Value: UnicodeString);
begin
  ChildNodes['fixed4x1'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed4x2: UnicodeString;
begin
  Result := ChildNodes['fixed4x2'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed4x2(Value: UnicodeString);
begin
  ChildNodes['fixed4x2'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed4x3: UnicodeString;
begin
  Result := ChildNodes['fixed4x3'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed4x3(Value: UnicodeString);
begin
  ChildNodes['fixed4x3'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Fixed4x4: UnicodeString;
begin
  Result := ChildNodes['fixed4x4'].Text;
end;

procedure TXMLCg_array_type.Set_Fixed4x4(Value: UnicodeString);
begin
  ChildNodes['fixed4x4'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLCg_array_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLCg_array_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLCg_array_type.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLCg_array_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLCg_array_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLCg_array_type.Get_String_: UnicodeString;
begin
  Result := ChildNodes['string'].Text;
end;

procedure TXMLCg_array_type.Set_String_(Value: UnicodeString);
begin
  ChildNodes['string'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLCg_array_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLCg_array_type.Get_Array_: IXMLCg_array_type;
begin
  Result := ChildNodes['array'] as IXMLCg_array_type;
end;

function TXMLCg_array_type.Get_Usertype: IXMLCg_user_type;
begin
  Result := ChildNodes['usertype'] as IXMLCg_user_type;
end;

{ TXMLCg_user_type }

procedure TXMLCg_user_type.AfterConstruction;
begin
  RegisterChildNode('setparam', TXMLCg_setparam_type);
  ItemTag := 'setparam';
  ItemInterface := IXMLCg_setparam_type;
  inherited;
end;

function TXMLCg_user_type.Get_Typename: UnicodeString;
begin
  Result := AttributeNodes['typename'].Text;
end;

procedure TXMLCg_user_type.Set_Typename(Value: UnicodeString);
begin
  SetAttribute('typename', Value);
end;

function TXMLCg_user_type.Get_Source: UnicodeString;
begin
  Result := AttributeNodes['source'].Text;
end;

procedure TXMLCg_user_type.Set_Source(Value: UnicodeString);
begin
  SetAttribute('source', Value);
end;

function TXMLCg_user_type.Get_Setparam(Index: Integer): IXMLCg_setparam_type;
begin
  Result := List[Index] as IXMLCg_setparam_type;
end;

function TXMLCg_user_type.Add: IXMLCg_setparam_type;
begin
  Result := AddItem(-1) as IXMLCg_setparam_type;
end;

function TXMLCg_user_type.Insert(const Index: Integer): IXMLCg_setparam_type;
begin
  Result := AddItem(Index) as IXMLCg_setparam_type;
end;

{ TXMLCg_setparam_type }

procedure TXMLCg_setparam_type.AfterConstruction;
begin
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLCg_array_type);
  RegisterChildNode('usertype', TXMLCg_user_type);
  inherited;
end;

function TXMLCg_setparam_type.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLCg_setparam_type.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

function TXMLCg_setparam_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLCg_setparam_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool2x1: UnicodeString;
begin
  Result := ChildNodes['bool2x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool2x1(Value: UnicodeString);
begin
  ChildNodes['bool2x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool2x2: UnicodeString;
begin
  Result := ChildNodes['bool2x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool2x2(Value: UnicodeString);
begin
  ChildNodes['bool2x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool2x3: UnicodeString;
begin
  Result := ChildNodes['bool2x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool2x3(Value: UnicodeString);
begin
  ChildNodes['bool2x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool2x4: UnicodeString;
begin
  Result := ChildNodes['bool2x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool2x4(Value: UnicodeString);
begin
  ChildNodes['bool2x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool3x1: UnicodeString;
begin
  Result := ChildNodes['bool3x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool3x1(Value: UnicodeString);
begin
  ChildNodes['bool3x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool3x2: UnicodeString;
begin
  Result := ChildNodes['bool3x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool3x2(Value: UnicodeString);
begin
  ChildNodes['bool3x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool3x3: UnicodeString;
begin
  Result := ChildNodes['bool3x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool3x3(Value: UnicodeString);
begin
  ChildNodes['bool3x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool3x4: UnicodeString;
begin
  Result := ChildNodes['bool3x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool3x4(Value: UnicodeString);
begin
  ChildNodes['bool3x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool4x1: UnicodeString;
begin
  Result := ChildNodes['bool4x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool4x1(Value: UnicodeString);
begin
  ChildNodes['bool4x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool4x2: UnicodeString;
begin
  Result := ChildNodes['bool4x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool4x2(Value: UnicodeString);
begin
  ChildNodes['bool4x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool4x3: UnicodeString;
begin
  Result := ChildNodes['bool4x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool4x3(Value: UnicodeString);
begin
  ChildNodes['bool4x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Bool4x4: UnicodeString;
begin
  Result := ChildNodes['bool4x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Bool4x4(Value: UnicodeString);
begin
  ChildNodes['bool4x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLCg_setparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float2x1: UnicodeString;
begin
  Result := ChildNodes['float2x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float2x1(Value: UnicodeString);
begin
  ChildNodes['float2x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float2x3: UnicodeString;
begin
  Result := ChildNodes['float2x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float2x3(Value: UnicodeString);
begin
  ChildNodes['float2x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float2x4: UnicodeString;
begin
  Result := ChildNodes['float2x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float2x4(Value: UnicodeString);
begin
  ChildNodes['float2x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float3x1: UnicodeString;
begin
  Result := ChildNodes['float3x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float3x1(Value: UnicodeString);
begin
  ChildNodes['float3x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float3x2: UnicodeString;
begin
  Result := ChildNodes['float3x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float3x2(Value: UnicodeString);
begin
  ChildNodes['float3x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float3x4: UnicodeString;
begin
  Result := ChildNodes['float3x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float3x4(Value: UnicodeString);
begin
  ChildNodes['float3x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float4x1: UnicodeString;
begin
  Result := ChildNodes['float4x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float4x1(Value: UnicodeString);
begin
  ChildNodes['float4x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float4x2: UnicodeString;
begin
  Result := ChildNodes['float4x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float4x2(Value: UnicodeString);
begin
  ChildNodes['float4x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float4x3: UnicodeString;
begin
  Result := ChildNodes['float4x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float4x3(Value: UnicodeString);
begin
  ChildNodes['float4x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLCg_setparam_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int2x1: UnicodeString;
begin
  Result := ChildNodes['int2x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int2x1(Value: UnicodeString);
begin
  ChildNodes['int2x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int2x2: UnicodeString;
begin
  Result := ChildNodes['int2x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int2x2(Value: UnicodeString);
begin
  ChildNodes['int2x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int2x3: UnicodeString;
begin
  Result := ChildNodes['int2x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int2x3(Value: UnicodeString);
begin
  ChildNodes['int2x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int2x4: UnicodeString;
begin
  Result := ChildNodes['int2x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int2x4(Value: UnicodeString);
begin
  ChildNodes['int2x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int3x1: UnicodeString;
begin
  Result := ChildNodes['int3x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int3x1(Value: UnicodeString);
begin
  ChildNodes['int3x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int3x2: UnicodeString;
begin
  Result := ChildNodes['int3x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int3x2(Value: UnicodeString);
begin
  ChildNodes['int3x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int3x3: UnicodeString;
begin
  Result := ChildNodes['int3x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int3x3(Value: UnicodeString);
begin
  ChildNodes['int3x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int3x4: UnicodeString;
begin
  Result := ChildNodes['int3x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int3x4(Value: UnicodeString);
begin
  ChildNodes['int3x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int4x1: UnicodeString;
begin
  Result := ChildNodes['int4x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int4x1(Value: UnicodeString);
begin
  ChildNodes['int4x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int4x2: UnicodeString;
begin
  Result := ChildNodes['int4x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int4x2(Value: UnicodeString);
begin
  ChildNodes['int4x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int4x3: UnicodeString;
begin
  Result := ChildNodes['int4x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int4x3(Value: UnicodeString);
begin
  ChildNodes['int4x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Int4x4: UnicodeString;
begin
  Result := ChildNodes['int4x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Int4x4(Value: UnicodeString);
begin
  ChildNodes['int4x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half: Double;
begin
  Result := ChildNodes['half'].NodeValue;
end;

procedure TXMLCg_setparam_type.Set_Half(Value: Double);
begin
  ChildNodes['half'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half2: UnicodeString;
begin
  Result := ChildNodes['half2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half2(Value: UnicodeString);
begin
  ChildNodes['half2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half3: UnicodeString;
begin
  Result := ChildNodes['half3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half3(Value: UnicodeString);
begin
  ChildNodes['half3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half4: UnicodeString;
begin
  Result := ChildNodes['half4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half4(Value: UnicodeString);
begin
  ChildNodes['half4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half2x1: UnicodeString;
begin
  Result := ChildNodes['half2x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half2x1(Value: UnicodeString);
begin
  ChildNodes['half2x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half2x2: UnicodeString;
begin
  Result := ChildNodes['half2x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half2x2(Value: UnicodeString);
begin
  ChildNodes['half2x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half2x3: UnicodeString;
begin
  Result := ChildNodes['half2x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half2x3(Value: UnicodeString);
begin
  ChildNodes['half2x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half2x4: UnicodeString;
begin
  Result := ChildNodes['half2x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half2x4(Value: UnicodeString);
begin
  ChildNodes['half2x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half3x1: UnicodeString;
begin
  Result := ChildNodes['half3x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half3x1(Value: UnicodeString);
begin
  ChildNodes['half3x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half3x2: UnicodeString;
begin
  Result := ChildNodes['half3x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half3x2(Value: UnicodeString);
begin
  ChildNodes['half3x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half3x3: UnicodeString;
begin
  Result := ChildNodes['half3x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half3x3(Value: UnicodeString);
begin
  ChildNodes['half3x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half3x4: UnicodeString;
begin
  Result := ChildNodes['half3x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half3x4(Value: UnicodeString);
begin
  ChildNodes['half3x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half4x1: UnicodeString;
begin
  Result := ChildNodes['half4x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half4x1(Value: UnicodeString);
begin
  ChildNodes['half4x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half4x2: UnicodeString;
begin
  Result := ChildNodes['half4x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half4x2(Value: UnicodeString);
begin
  ChildNodes['half4x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half4x3: UnicodeString;
begin
  Result := ChildNodes['half4x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half4x3(Value: UnicodeString);
begin
  ChildNodes['half4x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Half4x4: UnicodeString;
begin
  Result := ChildNodes['half4x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Half4x4(Value: UnicodeString);
begin
  ChildNodes['half4x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed: Double;
begin
  Result := ChildNodes['fixed'].NodeValue;
end;

procedure TXMLCg_setparam_type.Set_Fixed(Value: Double);
begin
  ChildNodes['fixed'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed2: UnicodeString;
begin
  Result := ChildNodes['fixed2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed2(Value: UnicodeString);
begin
  ChildNodes['fixed2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed3: UnicodeString;
begin
  Result := ChildNodes['fixed3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed3(Value: UnicodeString);
begin
  ChildNodes['fixed3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed4: UnicodeString;
begin
  Result := ChildNodes['fixed4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed4(Value: UnicodeString);
begin
  ChildNodes['fixed4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed2x1: UnicodeString;
begin
  Result := ChildNodes['fixed2x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed2x1(Value: UnicodeString);
begin
  ChildNodes['fixed2x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed2x2: UnicodeString;
begin
  Result := ChildNodes['fixed2x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed2x2(Value: UnicodeString);
begin
  ChildNodes['fixed2x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed2x3: UnicodeString;
begin
  Result := ChildNodes['fixed2x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed2x3(Value: UnicodeString);
begin
  ChildNodes['fixed2x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed2x4: UnicodeString;
begin
  Result := ChildNodes['fixed2x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed2x4(Value: UnicodeString);
begin
  ChildNodes['fixed2x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed3x1: UnicodeString;
begin
  Result := ChildNodes['fixed3x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed3x1(Value: UnicodeString);
begin
  ChildNodes['fixed3x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed3x2: UnicodeString;
begin
  Result := ChildNodes['fixed3x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed3x2(Value: UnicodeString);
begin
  ChildNodes['fixed3x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed3x3: UnicodeString;
begin
  Result := ChildNodes['fixed3x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed3x3(Value: UnicodeString);
begin
  ChildNodes['fixed3x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed3x4: UnicodeString;
begin
  Result := ChildNodes['fixed3x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed3x4(Value: UnicodeString);
begin
  ChildNodes['fixed3x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed4x1: UnicodeString;
begin
  Result := ChildNodes['fixed4x1'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed4x1(Value: UnicodeString);
begin
  ChildNodes['fixed4x1'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed4x2: UnicodeString;
begin
  Result := ChildNodes['fixed4x2'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed4x2(Value: UnicodeString);
begin
  ChildNodes['fixed4x2'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed4x3: UnicodeString;
begin
  Result := ChildNodes['fixed4x3'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed4x3(Value: UnicodeString);
begin
  ChildNodes['fixed4x3'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Fixed4x4: UnicodeString;
begin
  Result := ChildNodes['fixed4x4'].Text;
end;

procedure TXMLCg_setparam_type.Set_Fixed4x4(Value: UnicodeString);
begin
  ChildNodes['fixed4x4'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLCg_setparam_type.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLCg_setparam_type.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLCg_setparam_type.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLCg_setparam_type.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLCg_setparam_type.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLCg_setparam_type.Get_String_: UnicodeString;
begin
  Result := ChildNodes['string'].Text;
end;

procedure TXMLCg_setparam_type.Set_String_(Value: UnicodeString);
begin
  ChildNodes['string'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLCg_setparam_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLCg_setparam_type.Get_Array_: IXMLCg_array_type;
begin
  Result := ChildNodes['array'] as IXMLCg_array_type;
end;

function TXMLCg_setparam_type.Get_Usertype: IXMLCg_user_type;
begin
  Result := ChildNodes['usertype'] as IXMLCg_user_type;
end;

{ TXMLProfile_cg_type_technique }

procedure TXMLProfile_cg_type_technique.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('pass', TXMLCg_pass_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FPass := CreateCollection(TXMLCg_pass_typeList, IXMLCg_pass_type, 'pass') as IXMLCg_pass_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_cg_type_technique.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_cg_type_technique.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_cg_type_technique.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_cg_type_technique.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_cg_type_technique.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_cg_type_technique.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLProfile_cg_type_technique.Get_Pass: IXMLCg_pass_typeList;
begin
  Result := FPass;
end;

function TXMLProfile_cg_type_technique.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_cg_type_techniqueList }

function TXMLProfile_cg_type_techniqueList.Add: IXMLProfile_cg_type_technique;
begin
  Result := AddItem(-1) as IXMLProfile_cg_type_technique;
end;

function TXMLProfile_cg_type_techniqueList.Insert(const Index: Integer): IXMLProfile_cg_type_technique;
begin
  Result := AddItem(Index) as IXMLProfile_cg_type_technique;
end;

function TXMLProfile_cg_type_techniqueList.Get_Item(Index: Integer): IXMLProfile_cg_type_technique;
begin
  Result := List[Index] as IXMLProfile_cg_type_technique;
end;

{ TXMLCg_pass_type }

procedure TXMLCg_pass_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('states', TXMLCg_pass_type_states);
  RegisterChildNode('program', TXMLCg_pass_type_program);
  RegisterChildNode('evaluate', TXMLCg_pass_type_evaluate);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLCg_pass_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLCg_pass_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLCg_pass_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLCg_pass_type.Get_States: IXMLCg_pass_type_states;
begin
  Result := ChildNodes['states'] as IXMLCg_pass_type_states;
end;

function TXMLCg_pass_type.Get_Program_: IXMLCg_pass_type_program;
begin
  Result := ChildNodes['program'] as IXMLCg_pass_type_program;
end;

function TXMLCg_pass_type.Get_Evaluate: IXMLCg_pass_type_evaluate;
begin
  Result := ChildNodes['evaluate'] as IXMLCg_pass_type_evaluate;
end;

function TXMLCg_pass_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLCg_pass_typeList }

function TXMLCg_pass_typeList.Add: IXMLCg_pass_type;
begin
  Result := AddItem(-1) as IXMLCg_pass_type;
end;

function TXMLCg_pass_typeList.Insert(const Index: Integer): IXMLCg_pass_type;
begin
  Result := AddItem(Index) as IXMLCg_pass_type;
end;

function TXMLCg_pass_typeList.Get_Item(Index: Integer): IXMLCg_pass_type;
begin
  Result := List[Index] as IXMLCg_pass_type;
end;

{ TXMLCg_pass_type_states }

procedure TXMLCg_pass_type_states.AfterConstruction;
begin
  RegisterChildNode('alpha_func', TXMLAlpha_func);
  RegisterChildNode('blend_func', TXMLBlend_func);
  RegisterChildNode('blend_func_separate', TXMLBlend_func_separate);
  RegisterChildNode('blend_equation', TXMLBlend_equation);
  RegisterChildNode('blend_equation_separate', TXMLBlend_equation_separate);
  RegisterChildNode('color_material', TXMLColor_material);
  RegisterChildNode('cull_face', TXMLCull_face);
  RegisterChildNode('depth_func', TXMLDepth_func);
  RegisterChildNode('fog_mode', TXMLFog_mode);
  RegisterChildNode('fog_coord_src', TXMLFog_coord_src);
  RegisterChildNode('front_face', TXMLFront_face);
  RegisterChildNode('light_model_color_control', TXMLLight_model_color_control);
  RegisterChildNode('logic_op', TXMLLogic_op);
  RegisterChildNode('polygon_mode', TXMLPolygon_mode);
  RegisterChildNode('shade_model', TXMLShade_model);
  RegisterChildNode('stencil_func', TXMLStencil_func);
  RegisterChildNode('stencil_op', TXMLStencil_op);
  RegisterChildNode('stencil_func_separate', TXMLStencil_func_separate);
  RegisterChildNode('stencil_op_separate', TXMLStencil_op_separate);
  RegisterChildNode('stencil_mask_separate', TXMLStencil_mask_separate);
  RegisterChildNode('light_enable', TXMLLight_enable);
  RegisterChildNode('light_ambient', TXMLLight_ambient);
  RegisterChildNode('light_diffuse', TXMLLight_diffuse);
  RegisterChildNode('light_specular', TXMLLight_specular);
  RegisterChildNode('light_position', TXMLLight_position);
  RegisterChildNode('light_constant_attenuation', TXMLLight_constant_attenuation);
  RegisterChildNode('light_linear_attenuation', TXMLLight_linear_attenuation);
  RegisterChildNode('light_quadratic_attenuation', TXMLLight_quadratic_attenuation);
  RegisterChildNode('light_spot_cutoff', TXMLLight_spot_cutoff);
  RegisterChildNode('light_spot_direction', TXMLLight_spot_direction);
  RegisterChildNode('light_spot_exponent', TXMLLight_spot_exponent);
  RegisterChildNode('texture1D', TXMLTexture1D);
  RegisterChildNode('texture2D', TXMLTexture2D);
  RegisterChildNode('texture3D', TXMLTexture3D);
  RegisterChildNode('textureCUBE', TXMLTextureCUBE);
  RegisterChildNode('textureRECT', TXMLTextureRECT);
  RegisterChildNode('textureDEPTH', TXMLTextureDEPTH);
  RegisterChildNode('texture1D_enable', TXMLTexture1D_enable);
  RegisterChildNode('texture2D_enable', TXMLTexture2D_enable);
  RegisterChildNode('texture3D_enable', TXMLTexture3D_enable);
  RegisterChildNode('textureCUBE_enable', TXMLTextureCUBE_enable);
  RegisterChildNode('textureRECT_enable', TXMLTextureRECT_enable);
  RegisterChildNode('textureDEPTH_enable', TXMLTextureDEPTH_enable);
  RegisterChildNode('texture_env_color', TXMLTexture_env_color);
  RegisterChildNode('texture_env_mode', TXMLTexture_env_mode);
  RegisterChildNode('clip_plane', TXMLClip_plane);
  RegisterChildNode('clip_plane_enable', TXMLClip_plane_enable);
  RegisterChildNode('blend_color', TXMLBlend_color);
  RegisterChildNode('color_mask', TXMLColor_mask);
  RegisterChildNode('depth_bounds', TXMLDepth_bounds);
  RegisterChildNode('depth_mask', TXMLDepth_mask);
  RegisterChildNode('depth_range', TXMLDepth_range);
  RegisterChildNode('fog_density', TXMLFog_density);
  RegisterChildNode('fog_start', TXMLFog_start);
  RegisterChildNode('fog_end', TXMLFog_end);
  RegisterChildNode('fog_color', TXMLFog_color);
  RegisterChildNode('light_model_ambient', TXMLLight_model_ambient);
  RegisterChildNode('lighting_enable', TXMLLighting_enable);
  RegisterChildNode('line_stipple', TXMLLine_stipple);
  RegisterChildNode('line_width', TXMLLine_width);
  RegisterChildNode('material_ambient', TXMLMaterial_ambient);
  RegisterChildNode('material_diffuse', TXMLMaterial_diffuse);
  RegisterChildNode('material_emission', TXMLMaterial_emission);
  RegisterChildNode('material_shininess', TXMLMaterial_shininess);
  RegisterChildNode('material_specular', TXMLMaterial_specular);
  RegisterChildNode('model_view_matrix', TXMLModel_view_matrix);
  RegisterChildNode('point_distance_attenuation', TXMLPoint_distance_attenuation);
  RegisterChildNode('point_fade_threshold_size', TXMLPoint_fade_threshold_size);
  RegisterChildNode('point_size', TXMLPoint_size);
  RegisterChildNode('point_size_min', TXMLPoint_size_min);
  RegisterChildNode('point_size_max', TXMLPoint_size_max);
  RegisterChildNode('polygon_offset', TXMLPolygon_offset);
  RegisterChildNode('projection_matrix', TXMLProjection_matrix);
  RegisterChildNode('scissor', TXMLScissor);
  RegisterChildNode('stencil_mask', TXMLStencil_mask);
  RegisterChildNode('alpha_test_enable', TXMLAlpha_test_enable);
  RegisterChildNode('blend_enable', TXMLBlend_enable);
  RegisterChildNode('color_logic_op_enable', TXMLColor_logic_op_enable);
  RegisterChildNode('color_material_enable', TXMLColor_material_enable);
  RegisterChildNode('cull_face_enable', TXMLCull_face_enable);
  RegisterChildNode('depth_bounds_enable', TXMLDepth_bounds_enable);
  RegisterChildNode('depth_clamp_enable', TXMLDepth_clamp_enable);
  RegisterChildNode('depth_test_enable', TXMLDepth_test_enable);
  RegisterChildNode('dither_enable', TXMLDither_enable);
  RegisterChildNode('fog_enable', TXMLFog_enable);
  RegisterChildNode('light_model_local_viewer_enable', TXMLLight_model_local_viewer_enable);
  RegisterChildNode('light_model_two_side_enable', TXMLLight_model_two_side_enable);
  RegisterChildNode('line_smooth_enable', TXMLLine_smooth_enable);
  RegisterChildNode('line_stipple_enable', TXMLLine_stipple_enable);
  RegisterChildNode('logic_op_enable', TXMLLogic_op_enable);
  RegisterChildNode('multisample_enable', TXMLMultisample_enable);
  RegisterChildNode('normalize_enable', TXMLNormalize_enable);
  RegisterChildNode('point_smooth_enable', TXMLPoint_smooth_enable);
  RegisterChildNode('polygon_offset_fill_enable', TXMLPolygon_offset_fill_enable);
  RegisterChildNode('polygon_offset_line_enable', TXMLPolygon_offset_line_enable);
  RegisterChildNode('polygon_offset_point_enable', TXMLPolygon_offset_point_enable);
  RegisterChildNode('polygon_smooth_enable', TXMLPolygon_smooth_enable);
  RegisterChildNode('polygon_stipple_enable', TXMLPolygon_stipple_enable);
  RegisterChildNode('rescale_normal_enable', TXMLRescale_normal_enable);
  RegisterChildNode('sample_alpha_to_coverage_enable', TXMLSample_alpha_to_coverage_enable);
  RegisterChildNode('sample_alpha_to_one_enable', TXMLSample_alpha_to_one_enable);
  RegisterChildNode('sample_coverage_enable', TXMLSample_coverage_enable);
  RegisterChildNode('scissor_test_enable', TXMLScissor_test_enable);
  RegisterChildNode('stencil_test_enable', TXMLStencil_test_enable);
  inherited;
end;

function TXMLCg_pass_type_states.Get_Alpha_func: IXMLAlpha_func;
begin
  Result := ChildNodes['alpha_func'] as IXMLAlpha_func;
end;

function TXMLCg_pass_type_states.Get_Blend_func: IXMLBlend_func;
begin
  Result := ChildNodes['blend_func'] as IXMLBlend_func;
end;

function TXMLCg_pass_type_states.Get_Blend_func_separate: IXMLBlend_func_separate;
begin
  Result := ChildNodes['blend_func_separate'] as IXMLBlend_func_separate;
end;

function TXMLCg_pass_type_states.Get_Blend_equation: IXMLBlend_equation;
begin
  Result := ChildNodes['blend_equation'] as IXMLBlend_equation;
end;

function TXMLCg_pass_type_states.Get_Blend_equation_separate: IXMLBlend_equation_separate;
begin
  Result := ChildNodes['blend_equation_separate'] as IXMLBlend_equation_separate;
end;

function TXMLCg_pass_type_states.Get_Color_material: IXMLColor_material;
begin
  Result := ChildNodes['color_material'] as IXMLColor_material;
end;

function TXMLCg_pass_type_states.Get_Cull_face: IXMLCull_face;
begin
  Result := ChildNodes['cull_face'] as IXMLCull_face;
end;

function TXMLCg_pass_type_states.Get_Depth_func: IXMLDepth_func;
begin
  Result := ChildNodes['depth_func'] as IXMLDepth_func;
end;

function TXMLCg_pass_type_states.Get_Fog_mode: IXMLFog_mode;
begin
  Result := ChildNodes['fog_mode'] as IXMLFog_mode;
end;

function TXMLCg_pass_type_states.Get_Fog_coord_src: IXMLFog_coord_src;
begin
  Result := ChildNodes['fog_coord_src'] as IXMLFog_coord_src;
end;

function TXMLCg_pass_type_states.Get_Front_face: IXMLFront_face;
begin
  Result := ChildNodes['front_face'] as IXMLFront_face;
end;

function TXMLCg_pass_type_states.Get_Light_model_color_control: IXMLLight_model_color_control;
begin
  Result := ChildNodes['light_model_color_control'] as IXMLLight_model_color_control;
end;

function TXMLCg_pass_type_states.Get_Logic_op: IXMLLogic_op;
begin
  Result := ChildNodes['logic_op'] as IXMLLogic_op;
end;

function TXMLCg_pass_type_states.Get_Polygon_mode: IXMLPolygon_mode;
begin
  Result := ChildNodes['polygon_mode'] as IXMLPolygon_mode;
end;

function TXMLCg_pass_type_states.Get_Shade_model: IXMLShade_model;
begin
  Result := ChildNodes['shade_model'] as IXMLShade_model;
end;

function TXMLCg_pass_type_states.Get_Stencil_func: IXMLStencil_func;
begin
  Result := ChildNodes['stencil_func'] as IXMLStencil_func;
end;

function TXMLCg_pass_type_states.Get_Stencil_op: IXMLStencil_op;
begin
  Result := ChildNodes['stencil_op'] as IXMLStencil_op;
end;

function TXMLCg_pass_type_states.Get_Stencil_func_separate: IXMLStencil_func_separate;
begin
  Result := ChildNodes['stencil_func_separate'] as IXMLStencil_func_separate;
end;

function TXMLCg_pass_type_states.Get_Stencil_op_separate: IXMLStencil_op_separate;
begin
  Result := ChildNodes['stencil_op_separate'] as IXMLStencil_op_separate;
end;

function TXMLCg_pass_type_states.Get_Stencil_mask_separate: IXMLStencil_mask_separate;
begin
  Result := ChildNodes['stencil_mask_separate'] as IXMLStencil_mask_separate;
end;

function TXMLCg_pass_type_states.Get_Light_enable: IXMLLight_enable;
begin
  Result := ChildNodes['light_enable'] as IXMLLight_enable;
end;

function TXMLCg_pass_type_states.Get_Light_ambient: IXMLLight_ambient;
begin
  Result := ChildNodes['light_ambient'] as IXMLLight_ambient;
end;

function TXMLCg_pass_type_states.Get_Light_diffuse: IXMLLight_diffuse;
begin
  Result := ChildNodes['light_diffuse'] as IXMLLight_diffuse;
end;

function TXMLCg_pass_type_states.Get_Light_specular: IXMLLight_specular;
begin
  Result := ChildNodes['light_specular'] as IXMLLight_specular;
end;

function TXMLCg_pass_type_states.Get_Light_position: IXMLLight_position;
begin
  Result := ChildNodes['light_position'] as IXMLLight_position;
end;

function TXMLCg_pass_type_states.Get_Light_constant_attenuation: IXMLLight_constant_attenuation;
begin
  Result := ChildNodes['light_constant_attenuation'] as IXMLLight_constant_attenuation;
end;

function TXMLCg_pass_type_states.Get_Light_linear_attenuation: IXMLLight_linear_attenuation;
begin
  Result := ChildNodes['light_linear_attenuation'] as IXMLLight_linear_attenuation;
end;

function TXMLCg_pass_type_states.Get_Light_quadratic_attenuation: IXMLLight_quadratic_attenuation;
begin
  Result := ChildNodes['light_quadratic_attenuation'] as IXMLLight_quadratic_attenuation;
end;

function TXMLCg_pass_type_states.Get_Light_spot_cutoff: IXMLLight_spot_cutoff;
begin
  Result := ChildNodes['light_spot_cutoff'] as IXMLLight_spot_cutoff;
end;

function TXMLCg_pass_type_states.Get_Light_spot_direction: IXMLLight_spot_direction;
begin
  Result := ChildNodes['light_spot_direction'] as IXMLLight_spot_direction;
end;

function TXMLCg_pass_type_states.Get_Light_spot_exponent: IXMLLight_spot_exponent;
begin
  Result := ChildNodes['light_spot_exponent'] as IXMLLight_spot_exponent;
end;

function TXMLCg_pass_type_states.Get_Texture1D: IXMLTexture1D;
begin
  Result := ChildNodes['texture1D'] as IXMLTexture1D;
end;

function TXMLCg_pass_type_states.Get_Texture2D: IXMLTexture2D;
begin
  Result := ChildNodes['texture2D'] as IXMLTexture2D;
end;

function TXMLCg_pass_type_states.Get_Texture3D: IXMLTexture3D;
begin
  Result := ChildNodes['texture3D'] as IXMLTexture3D;
end;

function TXMLCg_pass_type_states.Get_TextureCUBE: IXMLTextureCUBE;
begin
  Result := ChildNodes['textureCUBE'] as IXMLTextureCUBE;
end;

function TXMLCg_pass_type_states.Get_TextureRECT: IXMLTextureRECT;
begin
  Result := ChildNodes['textureRECT'] as IXMLTextureRECT;
end;

function TXMLCg_pass_type_states.Get_TextureDEPTH: IXMLTextureDEPTH;
begin
  Result := ChildNodes['textureDEPTH'] as IXMLTextureDEPTH;
end;

function TXMLCg_pass_type_states.Get_Texture1D_enable: IXMLTexture1D_enable;
begin
  Result := ChildNodes['texture1D_enable'] as IXMLTexture1D_enable;
end;

function TXMLCg_pass_type_states.Get_Texture2D_enable: IXMLTexture2D_enable;
begin
  Result := ChildNodes['texture2D_enable'] as IXMLTexture2D_enable;
end;

function TXMLCg_pass_type_states.Get_Texture3D_enable: IXMLTexture3D_enable;
begin
  Result := ChildNodes['texture3D_enable'] as IXMLTexture3D_enable;
end;

function TXMLCg_pass_type_states.Get_TextureCUBE_enable: IXMLTextureCUBE_enable;
begin
  Result := ChildNodes['textureCUBE_enable'] as IXMLTextureCUBE_enable;
end;

function TXMLCg_pass_type_states.Get_TextureRECT_enable: IXMLTextureRECT_enable;
begin
  Result := ChildNodes['textureRECT_enable'] as IXMLTextureRECT_enable;
end;

function TXMLCg_pass_type_states.Get_TextureDEPTH_enable: IXMLTextureDEPTH_enable;
begin
  Result := ChildNodes['textureDEPTH_enable'] as IXMLTextureDEPTH_enable;
end;

function TXMLCg_pass_type_states.Get_Texture_env_color: IXMLTexture_env_color;
begin
  Result := ChildNodes['texture_env_color'] as IXMLTexture_env_color;
end;

function TXMLCg_pass_type_states.Get_Texture_env_mode: IXMLTexture_env_mode;
begin
  Result := ChildNodes['texture_env_mode'] as IXMLTexture_env_mode;
end;

function TXMLCg_pass_type_states.Get_Clip_plane: IXMLClip_plane;
begin
  Result := ChildNodes['clip_plane'] as IXMLClip_plane;
end;

function TXMLCg_pass_type_states.Get_Clip_plane_enable: IXMLClip_plane_enable;
begin
  Result := ChildNodes['clip_plane_enable'] as IXMLClip_plane_enable;
end;

function TXMLCg_pass_type_states.Get_Blend_color: IXMLBlend_color;
begin
  Result := ChildNodes['blend_color'] as IXMLBlend_color;
end;

function TXMLCg_pass_type_states.Get_Color_mask: IXMLColor_mask;
begin
  Result := ChildNodes['color_mask'] as IXMLColor_mask;
end;

function TXMLCg_pass_type_states.Get_Depth_bounds: IXMLDepth_bounds;
begin
  Result := ChildNodes['depth_bounds'] as IXMLDepth_bounds;
end;

function TXMLCg_pass_type_states.Get_Depth_mask: IXMLDepth_mask;
begin
  Result := ChildNodes['depth_mask'] as IXMLDepth_mask;
end;

function TXMLCg_pass_type_states.Get_Depth_range: IXMLDepth_range;
begin
  Result := ChildNodes['depth_range'] as IXMLDepth_range;
end;

function TXMLCg_pass_type_states.Get_Fog_density: IXMLFog_density;
begin
  Result := ChildNodes['fog_density'] as IXMLFog_density;
end;

function TXMLCg_pass_type_states.Get_Fog_start: IXMLFog_start;
begin
  Result := ChildNodes['fog_start'] as IXMLFog_start;
end;

function TXMLCg_pass_type_states.Get_Fog_end: IXMLFog_end;
begin
  Result := ChildNodes['fog_end'] as IXMLFog_end;
end;

function TXMLCg_pass_type_states.Get_Fog_color: IXMLFog_color;
begin
  Result := ChildNodes['fog_color'] as IXMLFog_color;
end;

function TXMLCg_pass_type_states.Get_Light_model_ambient: IXMLLight_model_ambient;
begin
  Result := ChildNodes['light_model_ambient'] as IXMLLight_model_ambient;
end;

function TXMLCg_pass_type_states.Get_Lighting_enable: IXMLLighting_enable;
begin
  Result := ChildNodes['lighting_enable'] as IXMLLighting_enable;
end;

function TXMLCg_pass_type_states.Get_Line_stipple: IXMLLine_stipple;
begin
  Result := ChildNodes['line_stipple'] as IXMLLine_stipple;
end;

function TXMLCg_pass_type_states.Get_Line_width: IXMLLine_width;
begin
  Result := ChildNodes['line_width'] as IXMLLine_width;
end;

function TXMLCg_pass_type_states.Get_Material_ambient: IXMLMaterial_ambient;
begin
  Result := ChildNodes['material_ambient'] as IXMLMaterial_ambient;
end;

function TXMLCg_pass_type_states.Get_Material_diffuse: IXMLMaterial_diffuse;
begin
  Result := ChildNodes['material_diffuse'] as IXMLMaterial_diffuse;
end;

function TXMLCg_pass_type_states.Get_Material_emission: IXMLMaterial_emission;
begin
  Result := ChildNodes['material_emission'] as IXMLMaterial_emission;
end;

function TXMLCg_pass_type_states.Get_Material_shininess: IXMLMaterial_shininess;
begin
  Result := ChildNodes['material_shininess'] as IXMLMaterial_shininess;
end;

function TXMLCg_pass_type_states.Get_Material_specular: IXMLMaterial_specular;
begin
  Result := ChildNodes['material_specular'] as IXMLMaterial_specular;
end;

function TXMLCg_pass_type_states.Get_Model_view_matrix: IXMLModel_view_matrix;
begin
  Result := ChildNodes['model_view_matrix'] as IXMLModel_view_matrix;
end;

function TXMLCg_pass_type_states.Get_Point_distance_attenuation: IXMLPoint_distance_attenuation;
begin
  Result := ChildNodes['point_distance_attenuation'] as IXMLPoint_distance_attenuation;
end;

function TXMLCg_pass_type_states.Get_Point_fade_threshold_size: IXMLPoint_fade_threshold_size;
begin
  Result := ChildNodes['point_fade_threshold_size'] as IXMLPoint_fade_threshold_size;
end;

function TXMLCg_pass_type_states.Get_Point_size: IXMLPoint_size;
begin
  Result := ChildNodes['point_size'] as IXMLPoint_size;
end;

function TXMLCg_pass_type_states.Get_Point_size_min: IXMLPoint_size_min;
begin
  Result := ChildNodes['point_size_min'] as IXMLPoint_size_min;
end;

function TXMLCg_pass_type_states.Get_Point_size_max: IXMLPoint_size_max;
begin
  Result := ChildNodes['point_size_max'] as IXMLPoint_size_max;
end;

function TXMLCg_pass_type_states.Get_Polygon_offset: IXMLPolygon_offset;
begin
  Result := ChildNodes['polygon_offset'] as IXMLPolygon_offset;
end;

function TXMLCg_pass_type_states.Get_Projection_matrix: IXMLProjection_matrix;
begin
  Result := ChildNodes['projection_matrix'] as IXMLProjection_matrix;
end;

function TXMLCg_pass_type_states.Get_Scissor: IXMLScissor;
begin
  Result := ChildNodes['scissor'] as IXMLScissor;
end;

function TXMLCg_pass_type_states.Get_Stencil_mask: IXMLStencil_mask;
begin
  Result := ChildNodes['stencil_mask'] as IXMLStencil_mask;
end;

function TXMLCg_pass_type_states.Get_Alpha_test_enable: IXMLAlpha_test_enable;
begin
  Result := ChildNodes['alpha_test_enable'] as IXMLAlpha_test_enable;
end;

function TXMLCg_pass_type_states.Get_Blend_enable: IXMLBlend_enable;
begin
  Result := ChildNodes['blend_enable'] as IXMLBlend_enable;
end;

function TXMLCg_pass_type_states.Get_Color_logic_op_enable: IXMLColor_logic_op_enable;
begin
  Result := ChildNodes['color_logic_op_enable'] as IXMLColor_logic_op_enable;
end;

function TXMLCg_pass_type_states.Get_Color_material_enable: IXMLColor_material_enable;
begin
  Result := ChildNodes['color_material_enable'] as IXMLColor_material_enable;
end;

function TXMLCg_pass_type_states.Get_Cull_face_enable: IXMLCull_face_enable;
begin
  Result := ChildNodes['cull_face_enable'] as IXMLCull_face_enable;
end;

function TXMLCg_pass_type_states.Get_Depth_bounds_enable: IXMLDepth_bounds_enable;
begin
  Result := ChildNodes['depth_bounds_enable'] as IXMLDepth_bounds_enable;
end;

function TXMLCg_pass_type_states.Get_Depth_clamp_enable: IXMLDepth_clamp_enable;
begin
  Result := ChildNodes['depth_clamp_enable'] as IXMLDepth_clamp_enable;
end;

function TXMLCg_pass_type_states.Get_Depth_test_enable: IXMLDepth_test_enable;
begin
  Result := ChildNodes['depth_test_enable'] as IXMLDepth_test_enable;
end;

function TXMLCg_pass_type_states.Get_Dither_enable: IXMLDither_enable;
begin
  Result := ChildNodes['dither_enable'] as IXMLDither_enable;
end;

function TXMLCg_pass_type_states.Get_Fog_enable: IXMLFog_enable;
begin
  Result := ChildNodes['fog_enable'] as IXMLFog_enable;
end;

function TXMLCg_pass_type_states.Get_Light_model_local_viewer_enable: IXMLLight_model_local_viewer_enable;
begin
  Result := ChildNodes['light_model_local_viewer_enable'] as IXMLLight_model_local_viewer_enable;
end;

function TXMLCg_pass_type_states.Get_Light_model_two_side_enable: IXMLLight_model_two_side_enable;
begin
  Result := ChildNodes['light_model_two_side_enable'] as IXMLLight_model_two_side_enable;
end;

function TXMLCg_pass_type_states.Get_Line_smooth_enable: IXMLLine_smooth_enable;
begin
  Result := ChildNodes['line_smooth_enable'] as IXMLLine_smooth_enable;
end;

function TXMLCg_pass_type_states.Get_Line_stipple_enable: IXMLLine_stipple_enable;
begin
  Result := ChildNodes['line_stipple_enable'] as IXMLLine_stipple_enable;
end;

function TXMLCg_pass_type_states.Get_Logic_op_enable: IXMLLogic_op_enable;
begin
  Result := ChildNodes['logic_op_enable'] as IXMLLogic_op_enable;
end;

function TXMLCg_pass_type_states.Get_Multisample_enable: IXMLMultisample_enable;
begin
  Result := ChildNodes['multisample_enable'] as IXMLMultisample_enable;
end;

function TXMLCg_pass_type_states.Get_Normalize_enable: IXMLNormalize_enable;
begin
  Result := ChildNodes['normalize_enable'] as IXMLNormalize_enable;
end;

function TXMLCg_pass_type_states.Get_Point_smooth_enable: IXMLPoint_smooth_enable;
begin
  Result := ChildNodes['point_smooth_enable'] as IXMLPoint_smooth_enable;
end;

function TXMLCg_pass_type_states.Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
begin
  Result := ChildNodes['polygon_offset_fill_enable'] as IXMLPolygon_offset_fill_enable;
end;

function TXMLCg_pass_type_states.Get_Polygon_offset_line_enable: IXMLPolygon_offset_line_enable;
begin
  Result := ChildNodes['polygon_offset_line_enable'] as IXMLPolygon_offset_line_enable;
end;

function TXMLCg_pass_type_states.Get_Polygon_offset_point_enable: IXMLPolygon_offset_point_enable;
begin
  Result := ChildNodes['polygon_offset_point_enable'] as IXMLPolygon_offset_point_enable;
end;

function TXMLCg_pass_type_states.Get_Polygon_smooth_enable: IXMLPolygon_smooth_enable;
begin
  Result := ChildNodes['polygon_smooth_enable'] as IXMLPolygon_smooth_enable;
end;

function TXMLCg_pass_type_states.Get_Polygon_stipple_enable: IXMLPolygon_stipple_enable;
begin
  Result := ChildNodes['polygon_stipple_enable'] as IXMLPolygon_stipple_enable;
end;

function TXMLCg_pass_type_states.Get_Rescale_normal_enable: IXMLRescale_normal_enable;
begin
  Result := ChildNodes['rescale_normal_enable'] as IXMLRescale_normal_enable;
end;

function TXMLCg_pass_type_states.Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
begin
  Result := ChildNodes['sample_alpha_to_coverage_enable'] as IXMLSample_alpha_to_coverage_enable;
end;

function TXMLCg_pass_type_states.Get_Sample_alpha_to_one_enable: IXMLSample_alpha_to_one_enable;
begin
  Result := ChildNodes['sample_alpha_to_one_enable'] as IXMLSample_alpha_to_one_enable;
end;

function TXMLCg_pass_type_states.Get_Sample_coverage_enable: IXMLSample_coverage_enable;
begin
  Result := ChildNodes['sample_coverage_enable'] as IXMLSample_coverage_enable;
end;

function TXMLCg_pass_type_states.Get_Scissor_test_enable: IXMLScissor_test_enable;
begin
  Result := ChildNodes['scissor_test_enable'] as IXMLScissor_test_enable;
end;

function TXMLCg_pass_type_states.Get_Stencil_test_enable: IXMLStencil_test_enable;
begin
  Result := ChildNodes['stencil_test_enable'] as IXMLStencil_test_enable;
end;

{ TXMLCg_pass_type_program }

procedure TXMLCg_pass_type_program.AfterConstruction;
begin
  RegisterChildNode('shader', TXMLCg_pass_type_program_shader);
  ItemTag := 'shader';
  ItemInterface := IXMLCg_pass_type_program_shader;
  inherited;
end;

function TXMLCg_pass_type_program.Get_Shader(Index: Integer): IXMLCg_pass_type_program_shader;
begin
  Result := List[Index] as IXMLCg_pass_type_program_shader;
end;

function TXMLCg_pass_type_program.Add: IXMLCg_pass_type_program_shader;
begin
  Result := AddItem(-1) as IXMLCg_pass_type_program_shader;
end;

function TXMLCg_pass_type_program.Insert(const Index: Integer): IXMLCg_pass_type_program_shader;
begin
  Result := AddItem(Index) as IXMLCg_pass_type_program_shader;
end;

{ TXMLCg_pass_type_program_shader }

procedure TXMLCg_pass_type_program_shader.AfterConstruction;
begin
  RegisterChildNode('sources', TXMLCg_pass_type_program_shader_sources);
  RegisterChildNode('compiler', TXMLFx_target_type);
  RegisterChildNode('bind_uniform', TXMLCg_pass_type_program_shader_bind_uniform);
  FCompiler := CreateCollection(TXMLFx_target_typeList, IXMLFx_target_type, 'compiler') as IXMLFx_target_typeList;
  FBind_uniform := CreateCollection(TXMLCg_pass_type_program_shader_bind_uniformList, IXMLCg_pass_type_program_shader_bind_uniform, 'bind_uniform') as IXMLCg_pass_type_program_shader_bind_uniformList;
  inherited;
end;

function TXMLCg_pass_type_program_shader.Get_Stage: UnicodeString;
begin
  Result := AttributeNodes['stage'].Text;
end;

procedure TXMLCg_pass_type_program_shader.Set_Stage(Value: UnicodeString);
begin
  SetAttribute('stage', Value);
end;

function TXMLCg_pass_type_program_shader.Get_Sources: IXMLCg_pass_type_program_shader_sources;
begin
  Result := ChildNodes['sources'] as IXMLCg_pass_type_program_shader_sources;
end;

function TXMLCg_pass_type_program_shader.Get_Compiler: IXMLFx_target_typeList;
begin
  Result := FCompiler;
end;

function TXMLCg_pass_type_program_shader.Get_Bind_uniform: IXMLCg_pass_type_program_shader_bind_uniformList;
begin
  Result := FBind_uniform;
end;

{ TXMLCg_pass_type_program_shader_sources }

function TXMLCg_pass_type_program_shader_sources.Get_Entry: UnicodeString;
begin
  Result := AttributeNodes['entry'].Text;
end;

procedure TXMLCg_pass_type_program_shader_sources.Set_Entry(Value: UnicodeString);
begin
  SetAttribute('entry', Value);
end;

{ TXMLCg_pass_type_program_shader_bind_uniform }

procedure TXMLCg_pass_type_program_shader_bind_uniform.AfterConstruction;
begin
  RegisterChildNode('param', TXMLCg_pass_type_program_shader_bind_uniform_param);
  RegisterChildNode('sampler1D', TXMLFx_sampler1D_type);
  RegisterChildNode('sampler2D', TXMLFx_sampler2D_type);
  RegisterChildNode('sampler3D', TXMLFx_sampler3D_type);
  RegisterChildNode('samplerRECT', TXMLFx_samplerRECT_type);
  RegisterChildNode('samplerCUBE', TXMLFx_samplerCUBE_type);
  RegisterChildNode('samplerDEPTH', TXMLFx_samplerDEPTH_type);
  RegisterChildNode('array', TXMLCg_array_type);
  RegisterChildNode('usertype', TXMLCg_user_type);
  inherited;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Symbol: UnicodeString;
begin
  Result := AttributeNodes['symbol'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Symbol(Value: UnicodeString);
begin
  SetAttribute('symbol', Value);
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Param: IXMLCg_pass_type_program_shader_bind_uniform_param;
begin
  Result := ChildNodes['param'] as IXMLCg_pass_type_program_shader_bind_uniform_param;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool2x1: UnicodeString;
begin
  Result := ChildNodes['bool2x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool2x1(Value: UnicodeString);
begin
  ChildNodes['bool2x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool2x2: UnicodeString;
begin
  Result := ChildNodes['bool2x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool2x2(Value: UnicodeString);
begin
  ChildNodes['bool2x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool2x3: UnicodeString;
begin
  Result := ChildNodes['bool2x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool2x3(Value: UnicodeString);
begin
  ChildNodes['bool2x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool2x4: UnicodeString;
begin
  Result := ChildNodes['bool2x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool2x4(Value: UnicodeString);
begin
  ChildNodes['bool2x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool3x1: UnicodeString;
begin
  Result := ChildNodes['bool3x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool3x1(Value: UnicodeString);
begin
  ChildNodes['bool3x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool3x2: UnicodeString;
begin
  Result := ChildNodes['bool3x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool3x2(Value: UnicodeString);
begin
  ChildNodes['bool3x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool3x3: UnicodeString;
begin
  Result := ChildNodes['bool3x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool3x3(Value: UnicodeString);
begin
  ChildNodes['bool3x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool3x4: UnicodeString;
begin
  Result := ChildNodes['bool3x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool3x4(Value: UnicodeString);
begin
  ChildNodes['bool3x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool4x1: UnicodeString;
begin
  Result := ChildNodes['bool4x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool4x1(Value: UnicodeString);
begin
  ChildNodes['bool4x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool4x2: UnicodeString;
begin
  Result := ChildNodes['bool4x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool4x2(Value: UnicodeString);
begin
  ChildNodes['bool4x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool4x3: UnicodeString;
begin
  Result := ChildNodes['bool4x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool4x3(Value: UnicodeString);
begin
  ChildNodes['bool4x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Bool4x4: UnicodeString;
begin
  Result := ChildNodes['bool4x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Bool4x4(Value: UnicodeString);
begin
  ChildNodes['bool4x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float2x1: UnicodeString;
begin
  Result := ChildNodes['float2x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float2x1(Value: UnicodeString);
begin
  ChildNodes['float2x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float2x3: UnicodeString;
begin
  Result := ChildNodes['float2x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float2x3(Value: UnicodeString);
begin
  ChildNodes['float2x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float2x4: UnicodeString;
begin
  Result := ChildNodes['float2x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float2x4(Value: UnicodeString);
begin
  ChildNodes['float2x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float3x1: UnicodeString;
begin
  Result := ChildNodes['float3x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float3x1(Value: UnicodeString);
begin
  ChildNodes['float3x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float3x2: UnicodeString;
begin
  Result := ChildNodes['float3x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float3x2(Value: UnicodeString);
begin
  ChildNodes['float3x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float3x4: UnicodeString;
begin
  Result := ChildNodes['float3x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float3x4(Value: UnicodeString);
begin
  ChildNodes['float3x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float4x1: UnicodeString;
begin
  Result := ChildNodes['float4x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float4x1(Value: UnicodeString);
begin
  ChildNodes['float4x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float4x2: UnicodeString;
begin
  Result := ChildNodes['float4x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float4x2(Value: UnicodeString);
begin
  ChildNodes['float4x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float4x3: UnicodeString;
begin
  Result := ChildNodes['float4x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float4x3(Value: UnicodeString);
begin
  ChildNodes['float4x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int2x1: UnicodeString;
begin
  Result := ChildNodes['int2x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int2x1(Value: UnicodeString);
begin
  ChildNodes['int2x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int2x2: UnicodeString;
begin
  Result := ChildNodes['int2x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int2x2(Value: UnicodeString);
begin
  ChildNodes['int2x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int2x3: UnicodeString;
begin
  Result := ChildNodes['int2x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int2x3(Value: UnicodeString);
begin
  ChildNodes['int2x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int2x4: UnicodeString;
begin
  Result := ChildNodes['int2x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int2x4(Value: UnicodeString);
begin
  ChildNodes['int2x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int3x1: UnicodeString;
begin
  Result := ChildNodes['int3x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int3x1(Value: UnicodeString);
begin
  ChildNodes['int3x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int3x2: UnicodeString;
begin
  Result := ChildNodes['int3x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int3x2(Value: UnicodeString);
begin
  ChildNodes['int3x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int3x3: UnicodeString;
begin
  Result := ChildNodes['int3x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int3x3(Value: UnicodeString);
begin
  ChildNodes['int3x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int3x4: UnicodeString;
begin
  Result := ChildNodes['int3x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int3x4(Value: UnicodeString);
begin
  ChildNodes['int3x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int4x1: UnicodeString;
begin
  Result := ChildNodes['int4x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int4x1(Value: UnicodeString);
begin
  ChildNodes['int4x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int4x2: UnicodeString;
begin
  Result := ChildNodes['int4x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int4x2(Value: UnicodeString);
begin
  ChildNodes['int4x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int4x3: UnicodeString;
begin
  Result := ChildNodes['int4x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int4x3(Value: UnicodeString);
begin
  ChildNodes['int4x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Int4x4: UnicodeString;
begin
  Result := ChildNodes['int4x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Int4x4(Value: UnicodeString);
begin
  ChildNodes['int4x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half: Double;
begin
  Result := ChildNodes['half'].NodeValue;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half(Value: Double);
begin
  ChildNodes['half'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half2: UnicodeString;
begin
  Result := ChildNodes['half2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half2(Value: UnicodeString);
begin
  ChildNodes['half2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half3: UnicodeString;
begin
  Result := ChildNodes['half3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half3(Value: UnicodeString);
begin
  ChildNodes['half3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half4: UnicodeString;
begin
  Result := ChildNodes['half4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half4(Value: UnicodeString);
begin
  ChildNodes['half4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half2x1: UnicodeString;
begin
  Result := ChildNodes['half2x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half2x1(Value: UnicodeString);
begin
  ChildNodes['half2x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half2x2: UnicodeString;
begin
  Result := ChildNodes['half2x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half2x2(Value: UnicodeString);
begin
  ChildNodes['half2x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half2x3: UnicodeString;
begin
  Result := ChildNodes['half2x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half2x3(Value: UnicodeString);
begin
  ChildNodes['half2x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half2x4: UnicodeString;
begin
  Result := ChildNodes['half2x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half2x4(Value: UnicodeString);
begin
  ChildNodes['half2x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half3x1: UnicodeString;
begin
  Result := ChildNodes['half3x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half3x1(Value: UnicodeString);
begin
  ChildNodes['half3x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half3x2: UnicodeString;
begin
  Result := ChildNodes['half3x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half3x2(Value: UnicodeString);
begin
  ChildNodes['half3x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half3x3: UnicodeString;
begin
  Result := ChildNodes['half3x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half3x3(Value: UnicodeString);
begin
  ChildNodes['half3x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half3x4: UnicodeString;
begin
  Result := ChildNodes['half3x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half3x4(Value: UnicodeString);
begin
  ChildNodes['half3x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half4x1: UnicodeString;
begin
  Result := ChildNodes['half4x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half4x1(Value: UnicodeString);
begin
  ChildNodes['half4x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half4x2: UnicodeString;
begin
  Result := ChildNodes['half4x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half4x2(Value: UnicodeString);
begin
  ChildNodes['half4x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half4x3: UnicodeString;
begin
  Result := ChildNodes['half4x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half4x3(Value: UnicodeString);
begin
  ChildNodes['half4x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Half4x4: UnicodeString;
begin
  Result := ChildNodes['half4x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Half4x4(Value: UnicodeString);
begin
  ChildNodes['half4x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed: Double;
begin
  Result := ChildNodes['fixed'].NodeValue;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed(Value: Double);
begin
  ChildNodes['fixed'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed2: UnicodeString;
begin
  Result := ChildNodes['fixed2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed2(Value: UnicodeString);
begin
  ChildNodes['fixed2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed3: UnicodeString;
begin
  Result := ChildNodes['fixed3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed3(Value: UnicodeString);
begin
  ChildNodes['fixed3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed4: UnicodeString;
begin
  Result := ChildNodes['fixed4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed4(Value: UnicodeString);
begin
  ChildNodes['fixed4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed2x1: UnicodeString;
begin
  Result := ChildNodes['fixed2x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed2x1(Value: UnicodeString);
begin
  ChildNodes['fixed2x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed2x2: UnicodeString;
begin
  Result := ChildNodes['fixed2x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed2x2(Value: UnicodeString);
begin
  ChildNodes['fixed2x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed2x3: UnicodeString;
begin
  Result := ChildNodes['fixed2x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed2x3(Value: UnicodeString);
begin
  ChildNodes['fixed2x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed2x4: UnicodeString;
begin
  Result := ChildNodes['fixed2x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed2x4(Value: UnicodeString);
begin
  ChildNodes['fixed2x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed3x1: UnicodeString;
begin
  Result := ChildNodes['fixed3x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed3x1(Value: UnicodeString);
begin
  ChildNodes['fixed3x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed3x2: UnicodeString;
begin
  Result := ChildNodes['fixed3x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed3x2(Value: UnicodeString);
begin
  ChildNodes['fixed3x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed3x3: UnicodeString;
begin
  Result := ChildNodes['fixed3x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed3x3(Value: UnicodeString);
begin
  ChildNodes['fixed3x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed3x4: UnicodeString;
begin
  Result := ChildNodes['fixed3x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed3x4(Value: UnicodeString);
begin
  ChildNodes['fixed3x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed4x1: UnicodeString;
begin
  Result := ChildNodes['fixed4x1'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed4x1(Value: UnicodeString);
begin
  ChildNodes['fixed4x1'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed4x2: UnicodeString;
begin
  Result := ChildNodes['fixed4x2'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed4x2(Value: UnicodeString);
begin
  ChildNodes['fixed4x2'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed4x3: UnicodeString;
begin
  Result := ChildNodes['fixed4x3'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed4x3(Value: UnicodeString);
begin
  ChildNodes['fixed4x3'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Fixed4x4: UnicodeString;
begin
  Result := ChildNodes['fixed4x4'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Fixed4x4(Value: UnicodeString);
begin
  ChildNodes['fixed4x4'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Sampler1D: IXMLFx_sampler1D_type;
begin
  Result := ChildNodes['sampler1D'] as IXMLFx_sampler1D_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Sampler2D: IXMLFx_sampler2D_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLFx_sampler2D_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Sampler3D: IXMLFx_sampler3D_type;
begin
  Result := ChildNodes['sampler3D'] as IXMLFx_sampler3D_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_SamplerRECT: IXMLFx_samplerRECT_type;
begin
  Result := ChildNodes['samplerRECT'] as IXMLFx_samplerRECT_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_SamplerCUBE: IXMLFx_samplerCUBE_type;
begin
  Result := ChildNodes['samplerCUBE'] as IXMLFx_samplerCUBE_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_SamplerDEPTH: IXMLFx_samplerDEPTH_type;
begin
  Result := ChildNodes['samplerDEPTH'] as IXMLFx_samplerDEPTH_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_String_: UnicodeString;
begin
  Result := ChildNodes['string'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_String_(Value: UnicodeString);
begin
  ChildNodes['string'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Array_: IXMLCg_array_type;
begin
  Result := ChildNodes['array'] as IXMLCg_array_type;
end;

function TXMLCg_pass_type_program_shader_bind_uniform.Get_Usertype: IXMLCg_user_type;
begin
  Result := ChildNodes['usertype'] as IXMLCg_user_type;
end;

{ TXMLCg_pass_type_program_shader_bind_uniformList }

function TXMLCg_pass_type_program_shader_bind_uniformList.Add: IXMLCg_pass_type_program_shader_bind_uniform;
begin
  Result := AddItem(-1) as IXMLCg_pass_type_program_shader_bind_uniform;
end;

function TXMLCg_pass_type_program_shader_bind_uniformList.Insert(const Index: Integer): IXMLCg_pass_type_program_shader_bind_uniform;
begin
  Result := AddItem(Index) as IXMLCg_pass_type_program_shader_bind_uniform;
end;

function TXMLCg_pass_type_program_shader_bind_uniformList.Get_Item(Index: Integer): IXMLCg_pass_type_program_shader_bind_uniform;
begin
  Result := List[Index] as IXMLCg_pass_type_program_shader_bind_uniform;
end;

{ TXMLCg_pass_type_program_shader_bind_uniform_param }

function TXMLCg_pass_type_program_shader_bind_uniform_param.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLCg_pass_type_program_shader_bind_uniform_param.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLCg_pass_type_evaluate }

procedure TXMLCg_pass_type_evaluate.AfterConstruction;
begin
  RegisterChildNode('color_target', TXMLFx_colortarget_type);
  RegisterChildNode('depth_target', TXMLFx_depthtarget_type);
  RegisterChildNode('stencil_target', TXMLFx_stenciltarget_type);
  RegisterChildNode('color_clear', TXMLFx_clearcolor_type);
  RegisterChildNode('depth_clear', TXMLFx_cleardepth_type);
  RegisterChildNode('stencil_clear', TXMLFx_clearstencil_type);
  FColor_target := CreateCollection(TXMLFx_colortarget_typeList, IXMLFx_colortarget_type, 'color_target') as IXMLFx_colortarget_typeList;
  FDepth_target := CreateCollection(TXMLFx_depthtarget_typeList, IXMLFx_depthtarget_type, 'depth_target') as IXMLFx_depthtarget_typeList;
  FStencil_target := CreateCollection(TXMLFx_stenciltarget_typeList, IXMLFx_stenciltarget_type, 'stencil_target') as IXMLFx_stenciltarget_typeList;
  FColor_clear := CreateCollection(TXMLFx_clearcolor_typeList, IXMLFx_clearcolor_type, 'color_clear') as IXMLFx_clearcolor_typeList;
  FDepth_clear := CreateCollection(TXMLFx_cleardepth_typeList, IXMLFx_cleardepth_type, 'depth_clear') as IXMLFx_cleardepth_typeList;
  FStencil_clear := CreateCollection(TXMLFx_clearstencil_typeList, IXMLFx_clearstencil_type, 'stencil_clear') as IXMLFx_clearstencil_typeList;
  inherited;
end;

function TXMLCg_pass_type_evaluate.Get_Color_target: IXMLFx_colortarget_typeList;
begin
  Result := FColor_target;
end;

function TXMLCg_pass_type_evaluate.Get_Depth_target: IXMLFx_depthtarget_typeList;
begin
  Result := FDepth_target;
end;

function TXMLCg_pass_type_evaluate.Get_Stencil_target: IXMLFx_stenciltarget_typeList;
begin
  Result := FStencil_target;
end;

function TXMLCg_pass_type_evaluate.Get_Color_clear: IXMLFx_clearcolor_typeList;
begin
  Result := FColor_clear;
end;

function TXMLCg_pass_type_evaluate.Get_Depth_clear: IXMLFx_cleardepth_typeList;
begin
  Result := FDepth_clear;
end;

function TXMLCg_pass_type_evaluate.Get_Stencil_clear: IXMLFx_clearstencil_typeList;
begin
  Result := FStencil_clear;
end;

function TXMLCg_pass_type_evaluate.Get_Draw: UnicodeString;
begin
  Result := ChildNodes['draw'].Text;
end;

procedure TXMLCg_pass_type_evaluate.Set_Draw(Value: UnicodeString);
begin
  ChildNodes['draw'].NodeValue := Value;
end;

{ TXMLProfile_gles_type }

procedure TXMLProfile_gles_type.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('newparam', TXMLGles_newparam_type);
  RegisterChildNode('technique', TXMLProfile_gles_type_technique);
  RegisterChildNode('extra', TXMLExtra_type);
  FNewparam := CreateCollection(TXMLGles_newparam_typeList, IXMLGles_newparam_type, 'newparam') as IXMLGles_newparam_typeList;
  FTechnique := CreateCollection(TXMLProfile_gles_type_techniqueList, IXMLProfile_gles_type_technique, 'technique') as IXMLProfile_gles_type_techniqueList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_gles_type.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_gles_type.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_gles_type.Get_Platform: UnicodeString;
begin
  Result := AttributeNodes['platform'].Text;
end;

procedure TXMLProfile_gles_type.Set_Platform(Value: UnicodeString);
begin
  SetAttribute('platform', Value);
end;

function TXMLProfile_gles_type.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_gles_type.Get_Newparam: IXMLGles_newparam_typeList;
begin
  Result := FNewparam;
end;

function TXMLProfile_gles_type.Get_Technique: IXMLProfile_gles_type_techniqueList;
begin
  Result := FTechnique;
end;

function TXMLProfile_gles_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGles_newparam_type }

procedure TXMLGles_newparam_type.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('sampler2D', TXMLGles_sampler_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  inherited;
end;

function TXMLGles_newparam_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLGles_newparam_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLGles_newparam_type.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLGles_newparam_type.Get_Semantic: UnicodeString;
begin
  Result := ChildNodes['semantic'].Text;
end;

procedure TXMLGles_newparam_type.Set_Semantic(Value: UnicodeString);
begin
  ChildNodes['semantic'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Modifier: UnicodeString;
begin
  Result := ChildNodes['modifier'].Text;
end;

procedure TXMLGles_newparam_type.Set_Modifier(Value: UnicodeString);
begin
  ChildNodes['modifier'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Bool: Boolean;
begin
  Result := ChildNodes['bool'].NodeValue;
end;

procedure TXMLGles_newparam_type.Set_Bool(Value: Boolean);
begin
  ChildNodes['bool'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Bool2: UnicodeString;
begin
  Result := ChildNodes['bool2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Bool2(Value: UnicodeString);
begin
  ChildNodes['bool2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Bool3: UnicodeString;
begin
  Result := ChildNodes['bool3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Bool3(Value: UnicodeString);
begin
  ChildNodes['bool3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Bool4: UnicodeString;
begin
  Result := ChildNodes['bool4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Bool4(Value: UnicodeString);
begin
  ChildNodes['bool4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Int: Int64;
begin
  Result := ChildNodes['int'].NodeValue;
end;

procedure TXMLGles_newparam_type.Set_Int(Value: Int64);
begin
  ChildNodes['int'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Int2: UnicodeString;
begin
  Result := ChildNodes['int2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Int2(Value: UnicodeString);
begin
  ChildNodes['int2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Int3: UnicodeString;
begin
  Result := ChildNodes['int3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Int3(Value: UnicodeString);
begin
  ChildNodes['int3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Int4: UnicodeString;
begin
  Result := ChildNodes['int4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Int4(Value: UnicodeString);
begin
  ChildNodes['int4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float: Double;
begin
  Result := ChildNodes['float'].NodeValue;
end;

procedure TXMLGles_newparam_type.Set_Float(Value: Double);
begin
  ChildNodes['float'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float2: UnicodeString;
begin
  Result := ChildNodes['float2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float2(Value: UnicodeString);
begin
  ChildNodes['float2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float3: UnicodeString;
begin
  Result := ChildNodes['float3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float3(Value: UnicodeString);
begin
  ChildNodes['float3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float4: UnicodeString;
begin
  Result := ChildNodes['float4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float4(Value: UnicodeString);
begin
  ChildNodes['float4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float1x1: Double;
begin
  Result := ChildNodes['float1x1'].NodeValue;
end;

procedure TXMLGles_newparam_type.Set_Float1x1(Value: Double);
begin
  ChildNodes['float1x1'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float1x2: UnicodeString;
begin
  Result := ChildNodes['float1x2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float1x2(Value: UnicodeString);
begin
  ChildNodes['float1x2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float1x3: UnicodeString;
begin
  Result := ChildNodes['float1x3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float1x3(Value: UnicodeString);
begin
  ChildNodes['float1x3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float1x4: UnicodeString;
begin
  Result := ChildNodes['float1x4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float1x4(Value: UnicodeString);
begin
  ChildNodes['float1x4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float2x1: UnicodeString;
begin
  Result := ChildNodes['float2x1'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float2x1(Value: UnicodeString);
begin
  ChildNodes['float2x1'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float2x2: UnicodeString;
begin
  Result := ChildNodes['float2x2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float2x2(Value: UnicodeString);
begin
  ChildNodes['float2x2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float2x3: UnicodeString;
begin
  Result := ChildNodes['float2x3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float2x3(Value: UnicodeString);
begin
  ChildNodes['float2x3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float2x4: UnicodeString;
begin
  Result := ChildNodes['float2x4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float2x4(Value: UnicodeString);
begin
  ChildNodes['float2x4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float3x1: UnicodeString;
begin
  Result := ChildNodes['float3x1'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float3x1(Value: UnicodeString);
begin
  ChildNodes['float3x1'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float3x2: UnicodeString;
begin
  Result := ChildNodes['float3x2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float3x2(Value: UnicodeString);
begin
  ChildNodes['float3x2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float3x3: UnicodeString;
begin
  Result := ChildNodes['float3x3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float3x3(Value: UnicodeString);
begin
  ChildNodes['float3x3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float3x4: UnicodeString;
begin
  Result := ChildNodes['float3x4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float3x4(Value: UnicodeString);
begin
  ChildNodes['float3x4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float4x1: UnicodeString;
begin
  Result := ChildNodes['float4x1'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float4x1(Value: UnicodeString);
begin
  ChildNodes['float4x1'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float4x2: UnicodeString;
begin
  Result := ChildNodes['float4x2'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float4x2(Value: UnicodeString);
begin
  ChildNodes['float4x2'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float4x3: UnicodeString;
begin
  Result := ChildNodes['float4x3'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float4x3(Value: UnicodeString);
begin
  ChildNodes['float4x3'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Float4x4: UnicodeString;
begin
  Result := ChildNodes['float4x4'].Text;
end;

procedure TXMLGles_newparam_type.Set_Float4x4(Value: UnicodeString);
begin
  ChildNodes['float4x4'].NodeValue := Value;
end;

function TXMLGles_newparam_type.Get_Sampler2D: IXMLGles_sampler_type;
begin
  Result := ChildNodes['sampler2D'] as IXMLGles_sampler_type;
end;

function TXMLGles_newparam_type.Get_Enum: UnicodeString;
begin
  Result := ChildNodes['enum'].Text;
end;

procedure TXMLGles_newparam_type.Set_Enum(Value: UnicodeString);
begin
  ChildNodes['enum'].NodeValue := Value;
end;

{ TXMLGles_newparam_typeList }

function TXMLGles_newparam_typeList.Add: IXMLGles_newparam_type;
begin
  Result := AddItem(-1) as IXMLGles_newparam_type;
end;

function TXMLGles_newparam_typeList.Insert(const Index: Integer): IXMLGles_newparam_type;
begin
  Result := AddItem(Index) as IXMLGles_newparam_type;
end;

function TXMLGles_newparam_typeList.Get_Item(Index: Integer): IXMLGles_newparam_type;
begin
  Result := List[Index] as IXMLGles_newparam_type;
end;

{ TXMLGles_sampler_type }

procedure TXMLGles_sampler_type.AfterConstruction;
begin
  RegisterChildNode('instance_image', TXMLInstance_image_type);
  RegisterChildNode('texcoord', TXMLGles_sampler_type_texcoord);
  RegisterChildNode('extra', TXMLExtra_type);
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLGles_sampler_type.Get_Instance_image: IXMLInstance_image_type;
begin
  Result := ChildNodes['instance_image'] as IXMLInstance_image_type;
end;

function TXMLGles_sampler_type.Get_Texcoord: IXMLGles_sampler_type_texcoord;
begin
  Result := ChildNodes['texcoord'] as IXMLGles_sampler_type_texcoord;
end;

function TXMLGles_sampler_type.Get_Wrap_s: UnicodeString;
begin
  Result := ChildNodes['wrap_s'].Text;
end;

procedure TXMLGles_sampler_type.Set_Wrap_s(Value: UnicodeString);
begin
  ChildNodes['wrap_s'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Wrap_t: UnicodeString;
begin
  Result := ChildNodes['wrap_t'].Text;
end;

procedure TXMLGles_sampler_type.Set_Wrap_t(Value: UnicodeString);
begin
  ChildNodes['wrap_t'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Minfilter: UnicodeString;
begin
  Result := ChildNodes['minfilter'].Text;
end;

procedure TXMLGles_sampler_type.Set_Minfilter(Value: UnicodeString);
begin
  ChildNodes['minfilter'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Magfilter: UnicodeString;
begin
  Result := ChildNodes['magfilter'].Text;
end;

procedure TXMLGles_sampler_type.Set_Magfilter(Value: UnicodeString);
begin
  ChildNodes['magfilter'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Mipfilter: UnicodeString;
begin
  Result := ChildNodes['mipfilter'].Text;
end;

procedure TXMLGles_sampler_type.Set_Mipfilter(Value: UnicodeString);
begin
  ChildNodes['mipfilter'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Mip_max_level: Byte;
begin
  Result := ChildNodes['mip_max_level'].NodeValue;
end;

procedure TXMLGles_sampler_type.Set_Mip_max_level(Value: Byte);
begin
  ChildNodes['mip_max_level'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Mip_bias: Single;
begin
  Result := ChildNodes['mip_bias'].NodeValue;
end;

procedure TXMLGles_sampler_type.Set_Mip_bias(Value: Single);
begin
  ChildNodes['mip_bias'].NodeValue := Value;
end;

function TXMLGles_sampler_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGles_sampler_type_texcoord }

function TXMLGles_sampler_type_texcoord.Get_Semantic: UnicodeString;
begin
  Result := AttributeNodes['semantic'].Text;
end;

procedure TXMLGles_sampler_type_texcoord.Set_Semantic(Value: UnicodeString);
begin
  SetAttribute('semantic', Value);
end;

{ TXMLProfile_gles_type_technique }

procedure TXMLProfile_gles_type_technique.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('pass', TXMLProfile_gles_type_technique_pass);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FPass := CreateCollection(TXMLProfile_gles_type_technique_passList, IXMLProfile_gles_type_technique_pass, 'pass') as IXMLProfile_gles_type_technique_passList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_gles_type_technique.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_gles_type_technique.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_gles_type_technique.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_gles_type_technique.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_gles_type_technique.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_gles_type_technique.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLProfile_gles_type_technique.Get_Pass: IXMLProfile_gles_type_technique_passList;
begin
  Result := FPass;
end;

function TXMLProfile_gles_type_technique.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_gles_type_techniqueList }

function TXMLProfile_gles_type_techniqueList.Add: IXMLProfile_gles_type_technique;
begin
  Result := AddItem(-1) as IXMLProfile_gles_type_technique;
end;

function TXMLProfile_gles_type_techniqueList.Insert(const Index: Integer): IXMLProfile_gles_type_technique;
begin
  Result := AddItem(Index) as IXMLProfile_gles_type_technique;
end;

function TXMLProfile_gles_type_techniqueList.Get_Item(Index: Integer): IXMLProfile_gles_type_technique;
begin
  Result := List[Index] as IXMLProfile_gles_type_technique;
end;

{ TXMLProfile_gles_type_technique_pass }

procedure TXMLProfile_gles_type_technique_pass.AfterConstruction;
begin
  RegisterChildNode('annotate', TXMLFx_annotate_type);
  RegisterChildNode('states', TXMLProfile_gles_type_technique_pass_states);
  RegisterChildNode('evaluate', TXMLProfile_gles_type_technique_pass_evaluate);
  RegisterChildNode('extra', TXMLExtra_type);
  FAnnotate := CreateCollection(TXMLFx_annotate_typeList, IXMLFx_annotate_type, 'annotate') as IXMLFx_annotate_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_gles_type_technique_pass.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_gles_type_technique_pass.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_gles_type_technique_pass.Get_Annotate: IXMLFx_annotate_typeList;
begin
  Result := FAnnotate;
end;

function TXMLProfile_gles_type_technique_pass.Get_States: IXMLProfile_gles_type_technique_pass_states;
begin
  Result := ChildNodes['states'] as IXMLProfile_gles_type_technique_pass_states;
end;

function TXMLProfile_gles_type_technique_pass.Get_Evaluate: IXMLProfile_gles_type_technique_pass_evaluate;
begin
  Result := ChildNodes['evaluate'] as IXMLProfile_gles_type_technique_pass_evaluate;
end;

function TXMLProfile_gles_type_technique_pass.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_gles_type_technique_passList }

function TXMLProfile_gles_type_technique_passList.Add: IXMLProfile_gles_type_technique_pass;
begin
  Result := AddItem(-1) as IXMLProfile_gles_type_technique_pass;
end;

function TXMLProfile_gles_type_technique_passList.Insert(const Index: Integer): IXMLProfile_gles_type_technique_pass;
begin
  Result := AddItem(Index) as IXMLProfile_gles_type_technique_pass;
end;

function TXMLProfile_gles_type_technique_passList.Get_Item(Index: Integer): IXMLProfile_gles_type_technique_pass;
begin
  Result := List[Index] as IXMLProfile_gles_type_technique_pass;
end;

{ TXMLProfile_gles_type_technique_pass_states }

procedure TXMLProfile_gles_type_technique_pass_states.AfterConstruction;
begin
  RegisterChildNode('alpha_func', TXMLAlpha_func);
  RegisterChildNode('blend_func', TXMLBlend_func);
  RegisterChildNode('clip_plane', TXMLClip_plane);
  RegisterChildNode('color_mask', TXMLColor_mask);
  RegisterChildNode('cull_face', TXMLCull_face);
  RegisterChildNode('depth_func', TXMLDepth_func);
  RegisterChildNode('depth_mask', TXMLDepth_mask);
  RegisterChildNode('depth_range', TXMLDepth_range);
  RegisterChildNode('fog_color', TXMLFog_color);
  RegisterChildNode('fog_density', TXMLFog_density);
  RegisterChildNode('fog_mode', TXMLFog_mode);
  RegisterChildNode('fog_start', TXMLFog_start);
  RegisterChildNode('fog_end', TXMLFog_end);
  RegisterChildNode('front_face', TXMLFront_face);
  RegisterChildNode('logic_op', TXMLLogic_op);
  RegisterChildNode('light_ambient', TXMLLight_ambient);
  RegisterChildNode('light_diffuse', TXMLLight_diffuse);
  RegisterChildNode('light_specular', TXMLLight_specular);
  RegisterChildNode('light_position', TXMLLight_position);
  RegisterChildNode('light_constant_attenuation', TXMLLight_constant_attenuation);
  RegisterChildNode('light_linear_attenuation', TXMLLight_linear_attenuation);
  RegisterChildNode('light_quadratic_attenuation', TXMLLight_quadratic_attenuation);
  RegisterChildNode('light_spot_cutoff', TXMLLight_spot_cutoff);
  RegisterChildNode('light_spot_direction', TXMLLight_spot_direction);
  RegisterChildNode('light_spot_exponent', TXMLLight_spot_exponent);
  RegisterChildNode('light_model_ambient', TXMLLight_model_ambient);
  RegisterChildNode('line_width', TXMLLine_width);
  RegisterChildNode('material_ambient', TXMLMaterial_ambient);
  RegisterChildNode('material_diffuse', TXMLMaterial_diffuse);
  RegisterChildNode('material_emission', TXMLMaterial_emission);
  RegisterChildNode('material_shininess', TXMLMaterial_shininess);
  RegisterChildNode('material_specular', TXMLMaterial_specular);
  RegisterChildNode('model_view_matrix', TXMLModel_view_matrix);
  RegisterChildNode('point_distance_attenuation', TXMLPoint_distance_attenuation);
  RegisterChildNode('point_fade_threshold_size', TXMLPoint_fade_threshold_size);
  RegisterChildNode('point_size', TXMLPoint_size);
  RegisterChildNode('point_size_min', TXMLPoint_size_min);
  RegisterChildNode('point_size_max', TXMLPoint_size_max);
  RegisterChildNode('polygon_offset', TXMLPolygon_offset);
  RegisterChildNode('projection_matrix', TXMLProjection_matrix);
  RegisterChildNode('scissor', TXMLScissor);
  RegisterChildNode('shade_model', TXMLShade_model);
  RegisterChildNode('stencil_func', TXMLStencil_func);
  RegisterChildNode('stencil_mask', TXMLStencil_mask);
  RegisterChildNode('stencil_op', TXMLStencil_op);
  RegisterChildNode('texture_pipeline', TXMLTexture_pipeline);
  RegisterChildNode('alpha_test_enable', TXMLAlpha_test_enable);
  RegisterChildNode('blend_enable', TXMLBlend_enable);
  RegisterChildNode('clip_plane_enable', TXMLClip_plane_enable);
  RegisterChildNode('color_logic_op_enable', TXMLColor_logic_op_enable);
  RegisterChildNode('color_material_enable', TXMLColor_material_enable);
  RegisterChildNode('cull_face_enable', TXMLCull_face_enable);
  RegisterChildNode('depth_test_enable', TXMLDepth_test_enable);
  RegisterChildNode('dither_enable', TXMLDither_enable);
  RegisterChildNode('fog_enable', TXMLFog_enable);
  RegisterChildNode('light_enable', TXMLLight_enable);
  RegisterChildNode('lighting_enable', TXMLLighting_enable);
  RegisterChildNode('light_model_two_side_enable', TXMLLight_model_two_side_enable);
  RegisterChildNode('line_smooth_enable', TXMLLine_smooth_enable);
  RegisterChildNode('multisample_enable', TXMLMultisample_enable);
  RegisterChildNode('normalize_enable', TXMLNormalize_enable);
  RegisterChildNode('point_smooth_enable', TXMLPoint_smooth_enable);
  RegisterChildNode('polygon_offset_fill_enable', TXMLPolygon_offset_fill_enable);
  RegisterChildNode('rescale_normal_enable', TXMLRescale_normal_enable);
  RegisterChildNode('sample_alpha_to_coverage_enable', TXMLSample_alpha_to_coverage_enable);
  RegisterChildNode('sample_alpha_to_one_enable', TXMLSample_alpha_to_one_enable);
  RegisterChildNode('sample_coverage_enable', TXMLSample_coverage_enable);
  RegisterChildNode('scissor_test_enable', TXMLScissor_test_enable);
  RegisterChildNode('stencil_test_enable', TXMLStencil_test_enable);
  inherited;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Alpha_func: IXMLAlpha_func;
begin
  Result := ChildNodes['alpha_func'] as IXMLAlpha_func;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Blend_func: IXMLBlend_func;
begin
  Result := ChildNodes['blend_func'] as IXMLBlend_func;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Clip_plane: IXMLClip_plane;
begin
  Result := ChildNodes['clip_plane'] as IXMLClip_plane;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Color_mask: IXMLColor_mask;
begin
  Result := ChildNodes['color_mask'] as IXMLColor_mask;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Cull_face: IXMLCull_face;
begin
  Result := ChildNodes['cull_face'] as IXMLCull_face;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Depth_func: IXMLDepth_func;
begin
  Result := ChildNodes['depth_func'] as IXMLDepth_func;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Depth_mask: IXMLDepth_mask;
begin
  Result := ChildNodes['depth_mask'] as IXMLDepth_mask;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Depth_range: IXMLDepth_range;
begin
  Result := ChildNodes['depth_range'] as IXMLDepth_range;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Fog_color: IXMLFog_color;
begin
  Result := ChildNodes['fog_color'] as IXMLFog_color;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Fog_density: IXMLFog_density;
begin
  Result := ChildNodes['fog_density'] as IXMLFog_density;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Fog_mode: IXMLFog_mode;
begin
  Result := ChildNodes['fog_mode'] as IXMLFog_mode;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Fog_start: IXMLFog_start;
begin
  Result := ChildNodes['fog_start'] as IXMLFog_start;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Fog_end: IXMLFog_end;
begin
  Result := ChildNodes['fog_end'] as IXMLFog_end;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Front_face: IXMLFront_face;
begin
  Result := ChildNodes['front_face'] as IXMLFront_face;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Logic_op: IXMLLogic_op;
begin
  Result := ChildNodes['logic_op'] as IXMLLogic_op;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_ambient: IXMLLight_ambient;
begin
  Result := ChildNodes['light_ambient'] as IXMLLight_ambient;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_diffuse: IXMLLight_diffuse;
begin
  Result := ChildNodes['light_diffuse'] as IXMLLight_diffuse;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_specular: IXMLLight_specular;
begin
  Result := ChildNodes['light_specular'] as IXMLLight_specular;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_position: IXMLLight_position;
begin
  Result := ChildNodes['light_position'] as IXMLLight_position;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_constant_attenuation: IXMLLight_constant_attenuation;
begin
  Result := ChildNodes['light_constant_attenuation'] as IXMLLight_constant_attenuation;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_linear_attenuation: IXMLLight_linear_attenuation;
begin
  Result := ChildNodes['light_linear_attenuation'] as IXMLLight_linear_attenuation;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_quadratic_attenuation: IXMLLight_quadratic_attenuation;
begin
  Result := ChildNodes['light_quadratic_attenuation'] as IXMLLight_quadratic_attenuation;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_spot_cutoff: IXMLLight_spot_cutoff;
begin
  Result := ChildNodes['light_spot_cutoff'] as IXMLLight_spot_cutoff;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_spot_direction: IXMLLight_spot_direction;
begin
  Result := ChildNodes['light_spot_direction'] as IXMLLight_spot_direction;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_spot_exponent: IXMLLight_spot_exponent;
begin
  Result := ChildNodes['light_spot_exponent'] as IXMLLight_spot_exponent;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_model_ambient: IXMLLight_model_ambient;
begin
  Result := ChildNodes['light_model_ambient'] as IXMLLight_model_ambient;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Line_width: IXMLLine_width;
begin
  Result := ChildNodes['line_width'] as IXMLLine_width;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Material_ambient: IXMLMaterial_ambient;
begin
  Result := ChildNodes['material_ambient'] as IXMLMaterial_ambient;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Material_diffuse: IXMLMaterial_diffuse;
begin
  Result := ChildNodes['material_diffuse'] as IXMLMaterial_diffuse;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Material_emission: IXMLMaterial_emission;
begin
  Result := ChildNodes['material_emission'] as IXMLMaterial_emission;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Material_shininess: IXMLMaterial_shininess;
begin
  Result := ChildNodes['material_shininess'] as IXMLMaterial_shininess;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Material_specular: IXMLMaterial_specular;
begin
  Result := ChildNodes['material_specular'] as IXMLMaterial_specular;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Model_view_matrix: IXMLModel_view_matrix;
begin
  Result := ChildNodes['model_view_matrix'] as IXMLModel_view_matrix;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Point_distance_attenuation: IXMLPoint_distance_attenuation;
begin
  Result := ChildNodes['point_distance_attenuation'] as IXMLPoint_distance_attenuation;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Point_fade_threshold_size: IXMLPoint_fade_threshold_size;
begin
  Result := ChildNodes['point_fade_threshold_size'] as IXMLPoint_fade_threshold_size;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Point_size: IXMLPoint_size;
begin
  Result := ChildNodes['point_size'] as IXMLPoint_size;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Point_size_min: IXMLPoint_size_min;
begin
  Result := ChildNodes['point_size_min'] as IXMLPoint_size_min;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Point_size_max: IXMLPoint_size_max;
begin
  Result := ChildNodes['point_size_max'] as IXMLPoint_size_max;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Polygon_offset: IXMLPolygon_offset;
begin
  Result := ChildNodes['polygon_offset'] as IXMLPolygon_offset;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Projection_matrix: IXMLProjection_matrix;
begin
  Result := ChildNodes['projection_matrix'] as IXMLProjection_matrix;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Scissor: IXMLScissor;
begin
  Result := ChildNodes['scissor'] as IXMLScissor;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Shade_model: IXMLShade_model;
begin
  Result := ChildNodes['shade_model'] as IXMLShade_model;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Stencil_func: IXMLStencil_func;
begin
  Result := ChildNodes['stencil_func'] as IXMLStencil_func;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Stencil_mask: IXMLStencil_mask;
begin
  Result := ChildNodes['stencil_mask'] as IXMLStencil_mask;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Stencil_op: IXMLStencil_op;
begin
  Result := ChildNodes['stencil_op'] as IXMLStencil_op;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Texture_pipeline: IXMLTexture_pipeline;
begin
  Result := ChildNodes['texture_pipeline'] as IXMLTexture_pipeline;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Alpha_test_enable: IXMLAlpha_test_enable;
begin
  Result := ChildNodes['alpha_test_enable'] as IXMLAlpha_test_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Blend_enable: IXMLBlend_enable;
begin
  Result := ChildNodes['blend_enable'] as IXMLBlend_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Clip_plane_enable: IXMLClip_plane_enable;
begin
  Result := ChildNodes['clip_plane_enable'] as IXMLClip_plane_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Color_logic_op_enable: IXMLColor_logic_op_enable;
begin
  Result := ChildNodes['color_logic_op_enable'] as IXMLColor_logic_op_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Color_material_enable: IXMLColor_material_enable;
begin
  Result := ChildNodes['color_material_enable'] as IXMLColor_material_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Cull_face_enable: IXMLCull_face_enable;
begin
  Result := ChildNodes['cull_face_enable'] as IXMLCull_face_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Depth_test_enable: IXMLDepth_test_enable;
begin
  Result := ChildNodes['depth_test_enable'] as IXMLDepth_test_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Dither_enable: IXMLDither_enable;
begin
  Result := ChildNodes['dither_enable'] as IXMLDither_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Fog_enable: IXMLFog_enable;
begin
  Result := ChildNodes['fog_enable'] as IXMLFog_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_enable: IXMLLight_enable;
begin
  Result := ChildNodes['light_enable'] as IXMLLight_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Lighting_enable: IXMLLighting_enable;
begin
  Result := ChildNodes['lighting_enable'] as IXMLLighting_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Light_model_two_side_enable: IXMLLight_model_two_side_enable;
begin
  Result := ChildNodes['light_model_two_side_enable'] as IXMLLight_model_two_side_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Line_smooth_enable: IXMLLine_smooth_enable;
begin
  Result := ChildNodes['line_smooth_enable'] as IXMLLine_smooth_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Multisample_enable: IXMLMultisample_enable;
begin
  Result := ChildNodes['multisample_enable'] as IXMLMultisample_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Normalize_enable: IXMLNormalize_enable;
begin
  Result := ChildNodes['normalize_enable'] as IXMLNormalize_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Point_smooth_enable: IXMLPoint_smooth_enable;
begin
  Result := ChildNodes['point_smooth_enable'] as IXMLPoint_smooth_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Polygon_offset_fill_enable: IXMLPolygon_offset_fill_enable;
begin
  Result := ChildNodes['polygon_offset_fill_enable'] as IXMLPolygon_offset_fill_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Rescale_normal_enable: IXMLRescale_normal_enable;
begin
  Result := ChildNodes['rescale_normal_enable'] as IXMLRescale_normal_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Sample_alpha_to_coverage_enable: IXMLSample_alpha_to_coverage_enable;
begin
  Result := ChildNodes['sample_alpha_to_coverage_enable'] as IXMLSample_alpha_to_coverage_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Sample_alpha_to_one_enable: IXMLSample_alpha_to_one_enable;
begin
  Result := ChildNodes['sample_alpha_to_one_enable'] as IXMLSample_alpha_to_one_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Sample_coverage_enable: IXMLSample_coverage_enable;
begin
  Result := ChildNodes['sample_coverage_enable'] as IXMLSample_coverage_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Scissor_test_enable: IXMLScissor_test_enable;
begin
  Result := ChildNodes['scissor_test_enable'] as IXMLScissor_test_enable;
end;

function TXMLProfile_gles_type_technique_pass_states.Get_Stencil_test_enable: IXMLStencil_test_enable;
begin
  Result := ChildNodes['stencil_test_enable'] as IXMLStencil_test_enable;
end;

{ TXMLTexture_pipeline }

procedure TXMLTexture_pipeline.AfterConstruction;
begin
  RegisterChildNode('value', TXMLGles_texture_pipeline_type);
  inherited;
end;

function TXMLTexture_pipeline.Get_Value: IXMLGles_texture_pipeline_type;
begin
  Result := ChildNodes['value'] as IXMLGles_texture_pipeline_type;
end;

{ TXMLGles_texture_pipeline_type }

procedure TXMLGles_texture_pipeline_type.AfterConstruction;
begin
  RegisterChildNode('texcombiner', TXMLGles_texcombiner_command_type);
  RegisterChildNode('texenv', TXMLGles_texenv_command_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FTexcombiner := CreateCollection(TXMLGles_texcombiner_command_typeList, IXMLGles_texcombiner_command_type, 'texcombiner') as IXMLGles_texcombiner_command_typeList;
  FTexenv := CreateCollection(TXMLGles_texenv_command_typeList, IXMLGles_texenv_command_type, 'texenv') as IXMLGles_texenv_command_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLGles_texture_pipeline_type.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLGles_texture_pipeline_type.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLGles_texture_pipeline_type.Get_Texcombiner: IXMLGles_texcombiner_command_typeList;
begin
  Result := FTexcombiner;
end;

function TXMLGles_texture_pipeline_type.Get_Texenv: IXMLGles_texenv_command_typeList;
begin
  Result := FTexenv;
end;

function TXMLGles_texture_pipeline_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLGles_texcombiner_command_type }

procedure TXMLGles_texcombiner_command_type.AfterConstruction;
begin
  RegisterChildNode('constant', TXMLGles_texture_constant_type);
  RegisterChildNode('RGB', TXMLGles_texcombiner_command_rgb_type);
  RegisterChildNode('alpha', TXMLGles_texcombiner_command_alpha_type);
  inherited;
end;

function TXMLGles_texcombiner_command_type.Get_Constant: IXMLGles_texture_constant_type;
begin
  Result := ChildNodes['constant'] as IXMLGles_texture_constant_type;
end;

function TXMLGles_texcombiner_command_type.Get_RGB: IXMLGles_texcombiner_command_rgb_type;
begin
  Result := ChildNodes['RGB'] as IXMLGles_texcombiner_command_rgb_type;
end;

function TXMLGles_texcombiner_command_type.Get_Alpha: IXMLGles_texcombiner_command_alpha_type;
begin
  Result := ChildNodes['alpha'] as IXMLGles_texcombiner_command_alpha_type;
end;

{ TXMLGles_texcombiner_command_typeList }

function TXMLGles_texcombiner_command_typeList.Add: IXMLGles_texcombiner_command_type;
begin
  Result := AddItem(-1) as IXMLGles_texcombiner_command_type;
end;

function TXMLGles_texcombiner_command_typeList.Insert(const Index: Integer): IXMLGles_texcombiner_command_type;
begin
  Result := AddItem(Index) as IXMLGles_texcombiner_command_type;
end;

function TXMLGles_texcombiner_command_typeList.Get_Item(Index: Integer): IXMLGles_texcombiner_command_type;
begin
  Result := List[Index] as IXMLGles_texcombiner_command_type;
end;

{ TXMLGles_texture_constant_type }

function TXMLGles_texture_constant_type.Get_Value: UnicodeString;
begin
  Result := AttributeNodes['value'].Text;
end;

procedure TXMLGles_texture_constant_type.Set_Value(Value: UnicodeString);
begin
  SetAttribute('value', Value);
end;

function TXMLGles_texture_constant_type.Get_Param: UnicodeString;
begin
  Result := AttributeNodes['param'].Text;
end;

procedure TXMLGles_texture_constant_type.Set_Param(Value: UnicodeString);
begin
  SetAttribute('param', Value);
end;

{ TXMLGles_texcombiner_command_rgb_type }

procedure TXMLGles_texcombiner_command_rgb_type.AfterConstruction;
begin
  RegisterChildNode('argument', TXMLGles_texcombiner_argument_rgb_type);
  ItemTag := 'argument';
  ItemInterface := IXMLGles_texcombiner_argument_rgb_type;
  inherited;
end;

function TXMLGles_texcombiner_command_rgb_type.Get_Operator_: UnicodeString;
begin
  Result := AttributeNodes['operator'].Text;
end;

procedure TXMLGles_texcombiner_command_rgb_type.Set_Operator_(Value: UnicodeString);
begin
  SetAttribute('operator', Value);
end;

function TXMLGles_texcombiner_command_rgb_type.Get_Scale: Single;
begin
  Result := AttributeNodes['scale'].NodeValue;
end;

procedure TXMLGles_texcombiner_command_rgb_type.Set_Scale(Value: Single);
begin
  SetAttribute('scale', Value);
end;

function TXMLGles_texcombiner_command_rgb_type.Get_Argument(Index: Integer): IXMLGles_texcombiner_argument_rgb_type;
begin
  Result := List[Index] as IXMLGles_texcombiner_argument_rgb_type;
end;

function TXMLGles_texcombiner_command_rgb_type.Add: IXMLGles_texcombiner_argument_rgb_type;
begin
  Result := AddItem(-1) as IXMLGles_texcombiner_argument_rgb_type;
end;

function TXMLGles_texcombiner_command_rgb_type.Insert(const Index: Integer): IXMLGles_texcombiner_argument_rgb_type;
begin
  Result := AddItem(Index) as IXMLGles_texcombiner_argument_rgb_type;
end;

{ TXMLGles_texcombiner_argument_rgb_type }

function TXMLGles_texcombiner_argument_rgb_type.Get_Source: UnicodeString;
begin
  Result := AttributeNodes['source'].Text;
end;

procedure TXMLGles_texcombiner_argument_rgb_type.Set_Source(Value: UnicodeString);
begin
  SetAttribute('source', Value);
end;

function TXMLGles_texcombiner_argument_rgb_type.Get_Operand: UnicodeString;
begin
  Result := AttributeNodes['operand'].Text;
end;

procedure TXMLGles_texcombiner_argument_rgb_type.Set_Operand(Value: UnicodeString);
begin
  SetAttribute('operand', Value);
end;

function TXMLGles_texcombiner_argument_rgb_type.Get_Sampler: UnicodeString;
begin
  Result := AttributeNodes['sampler'].Text;
end;

procedure TXMLGles_texcombiner_argument_rgb_type.Set_Sampler(Value: UnicodeString);
begin
  SetAttribute('sampler', Value);
end;

{ TXMLGles_texcombiner_command_alpha_type }

procedure TXMLGles_texcombiner_command_alpha_type.AfterConstruction;
begin
  RegisterChildNode('argument', TXMLGles_texcombiner_argument_alpha_type);
  ItemTag := 'argument';
  ItemInterface := IXMLGles_texcombiner_argument_alpha_type;
  inherited;
end;

function TXMLGles_texcombiner_command_alpha_type.Get_Operator_: UnicodeString;
begin
  Result := AttributeNodes['operator'].Text;
end;

procedure TXMLGles_texcombiner_command_alpha_type.Set_Operator_(Value: UnicodeString);
begin
  SetAttribute('operator', Value);
end;

function TXMLGles_texcombiner_command_alpha_type.Get_Scale: Single;
begin
  Result := AttributeNodes['scale'].NodeValue;
end;

procedure TXMLGles_texcombiner_command_alpha_type.Set_Scale(Value: Single);
begin
  SetAttribute('scale', Value);
end;

function TXMLGles_texcombiner_command_alpha_type.Get_Argument(Index: Integer): IXMLGles_texcombiner_argument_alpha_type;
begin
  Result := List[Index] as IXMLGles_texcombiner_argument_alpha_type;
end;

function TXMLGles_texcombiner_command_alpha_type.Add: IXMLGles_texcombiner_argument_alpha_type;
begin
  Result := AddItem(-1) as IXMLGles_texcombiner_argument_alpha_type;
end;

function TXMLGles_texcombiner_command_alpha_type.Insert(const Index: Integer): IXMLGles_texcombiner_argument_alpha_type;
begin
  Result := AddItem(Index) as IXMLGles_texcombiner_argument_alpha_type;
end;

{ TXMLGles_texcombiner_argument_alpha_type }

function TXMLGles_texcombiner_argument_alpha_type.Get_Source: UnicodeString;
begin
  Result := AttributeNodes['source'].Text;
end;

procedure TXMLGles_texcombiner_argument_alpha_type.Set_Source(Value: UnicodeString);
begin
  SetAttribute('source', Value);
end;

function TXMLGles_texcombiner_argument_alpha_type.Get_Operand: UnicodeString;
begin
  Result := AttributeNodes['operand'].Text;
end;

procedure TXMLGles_texcombiner_argument_alpha_type.Set_Operand(Value: UnicodeString);
begin
  SetAttribute('operand', Value);
end;

function TXMLGles_texcombiner_argument_alpha_type.Get_Sampler: UnicodeString;
begin
  Result := AttributeNodes['sampler'].Text;
end;

procedure TXMLGles_texcombiner_argument_alpha_type.Set_Sampler(Value: UnicodeString);
begin
  SetAttribute('sampler', Value);
end;

{ TXMLGles_texenv_command_type }

procedure TXMLGles_texenv_command_type.AfterConstruction;
begin
  RegisterChildNode('constant', TXMLGles_texture_constant_type);
  inherited;
end;

function TXMLGles_texenv_command_type.Get_Operator_: UnicodeString;
begin
  Result := AttributeNodes['operator'].Text;
end;

procedure TXMLGles_texenv_command_type.Set_Operator_(Value: UnicodeString);
begin
  SetAttribute('operator', Value);
end;

function TXMLGles_texenv_command_type.Get_Sampler: UnicodeString;
begin
  Result := AttributeNodes['sampler'].Text;
end;

procedure TXMLGles_texenv_command_type.Set_Sampler(Value: UnicodeString);
begin
  SetAttribute('sampler', Value);
end;

function TXMLGles_texenv_command_type.Get_Constant: IXMLGles_texture_constant_type;
begin
  Result := ChildNodes['constant'] as IXMLGles_texture_constant_type;
end;

{ TXMLGles_texenv_command_typeList }

function TXMLGles_texenv_command_typeList.Add: IXMLGles_texenv_command_type;
begin
  Result := AddItem(-1) as IXMLGles_texenv_command_type;
end;

function TXMLGles_texenv_command_typeList.Insert(const Index: Integer): IXMLGles_texenv_command_type;
begin
  Result := AddItem(Index) as IXMLGles_texenv_command_type;
end;

function TXMLGles_texenv_command_typeList.Get_Item(Index: Integer): IXMLGles_texenv_command_type;
begin
  Result := List[Index] as IXMLGles_texenv_command_type;
end;

{ TXMLProfile_gles_type_technique_pass_evaluate }

procedure TXMLProfile_gles_type_technique_pass_evaluate.AfterConstruction;
begin
  RegisterChildNode('color_target', TXMLFx_colortarget_type);
  RegisterChildNode('depth_target', TXMLFx_depthtarget_type);
  RegisterChildNode('stencil_target', TXMLFx_stenciltarget_type);
  RegisterChildNode('color_clear', TXMLFx_clearcolor_type);
  RegisterChildNode('depth_clear', TXMLFx_cleardepth_type);
  RegisterChildNode('stencil_clear', TXMLFx_clearstencil_type);
  FColor_target := CreateCollection(TXMLFx_colortarget_typeList, IXMLFx_colortarget_type, 'color_target') as IXMLFx_colortarget_typeList;
  FDepth_target := CreateCollection(TXMLFx_depthtarget_typeList, IXMLFx_depthtarget_type, 'depth_target') as IXMLFx_depthtarget_typeList;
  FStencil_target := CreateCollection(TXMLFx_stenciltarget_typeList, IXMLFx_stenciltarget_type, 'stencil_target') as IXMLFx_stenciltarget_typeList;
  FColor_clear := CreateCollection(TXMLFx_clearcolor_typeList, IXMLFx_clearcolor_type, 'color_clear') as IXMLFx_clearcolor_typeList;
  FDepth_clear := CreateCollection(TXMLFx_cleardepth_typeList, IXMLFx_cleardepth_type, 'depth_clear') as IXMLFx_cleardepth_typeList;
  FStencil_clear := CreateCollection(TXMLFx_clearstencil_typeList, IXMLFx_clearstencil_type, 'stencil_clear') as IXMLFx_clearstencil_typeList;
  inherited;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Color_target: IXMLFx_colortarget_typeList;
begin
  Result := FColor_target;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Depth_target: IXMLFx_depthtarget_typeList;
begin
  Result := FDepth_target;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Stencil_target: IXMLFx_stenciltarget_typeList;
begin
  Result := FStencil_target;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Color_clear: IXMLFx_clearcolor_typeList;
begin
  Result := FColor_clear;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Depth_clear: IXMLFx_cleardepth_typeList;
begin
  Result := FDepth_clear;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Stencil_clear: IXMLFx_clearstencil_typeList;
begin
  Result := FStencil_clear;
end;

function TXMLProfile_gles_type_technique_pass_evaluate.Get_Draw: UnicodeString;
begin
  Result := ChildNodes['draw'].Text;
end;

procedure TXMLProfile_gles_type_technique_pass_evaluate.Set_Draw(Value: UnicodeString);
begin
  ChildNodes['draw'].NodeValue := Value;
end;



{ TXMLProfile_common_type }

procedure TXMLProfile_common_type.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('newparam', TXMLFx_common_newparam_type);
  RegisterChildNode('technique', TXMLProfile_common_type_technique);
  RegisterChildNode('extra', TXMLExtra_type);
  FNewparam := CreateCollection(TXMLFx_common_newparam_typeList, IXMLFx_common_newparam_type, 'newparam') as IXMLFx_common_newparam_typeList;
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_common_type.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_common_type.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_common_type.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_common_type.Get_Newparam: IXMLFx_common_newparam_typeList;
begin
  Result := FNewparam;
end;

function TXMLProfile_common_type.Get_Technique: IXMLProfile_common_type_technique;
begin
  Result := ChildNodes['technique'] as IXMLProfile_common_type_technique;
end;

function TXMLProfile_common_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_bridge_type }

procedure TXMLProfile_bridge_type.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('extra', TXMLExtra_type);
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_bridge_type.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_bridge_type.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_bridge_type.Get_Platform: UnicodeString;
begin
  Result := AttributeNodes['platform'].Text;
end;

procedure TXMLProfile_bridge_type.Set_Platform(Value: UnicodeString);
begin
  SetAttribute('platform', Value);
end;

function TXMLProfile_bridge_type.Get_Url: UnicodeString;
begin
  Result := AttributeNodes['url'].Text;
end;

procedure TXMLProfile_bridge_type.Set_Url(Value: UnicodeString);
begin
  SetAttribute('url', Value);
end;

function TXMLProfile_bridge_type.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_bridge_type.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_common_type_technique }

procedure TXMLProfile_common_type_technique.AfterConstruction;
begin
  RegisterChildNode('asset', TXMLAsset_type);
  RegisterChildNode('constant', TXMLProfile_common_type_technique_constant);
  RegisterChildNode('lambert', TXMLProfile_common_type_technique_lambert);
  RegisterChildNode('phong', TXMLProfile_common_type_technique_phong);
  RegisterChildNode('blinn', TXMLProfile_common_type_technique_blinn);
  RegisterChildNode('extra', TXMLExtra_type);
  FExtra := CreateCollection(TXMLExtra_typeList, IXMLExtra_type, 'extra') as IXMLExtra_typeList;
  inherited;
end;

function TXMLProfile_common_type_technique.Get_Id: UnicodeString;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLProfile_common_type_technique.Set_Id(Value: UnicodeString);
begin
  SetAttribute('id', Value);
end;

function TXMLProfile_common_type_technique.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLProfile_common_type_technique.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

function TXMLProfile_common_type_technique.Get_Asset: IXMLAsset_type;
begin
  Result := ChildNodes['asset'] as IXMLAsset_type;
end;

function TXMLProfile_common_type_technique.Get_Constant: IXMLProfile_common_type_technique_constant;
begin
  Result := ChildNodes['constant'] as IXMLProfile_common_type_technique_constant;
end;

function TXMLProfile_common_type_technique.Get_Lambert: IXMLProfile_common_type_technique_lambert;
begin
  Result := ChildNodes['lambert'] as IXMLProfile_common_type_technique_lambert;
end;

function TXMLProfile_common_type_technique.Get_Phong: IXMLProfile_common_type_technique_phong;
begin
  Result := ChildNodes['phong'] as IXMLProfile_common_type_technique_phong;
end;

function TXMLProfile_common_type_technique.Get_Blinn: IXMLProfile_common_type_technique_blinn;
begin
  Result := ChildNodes['blinn'] as IXMLProfile_common_type_technique_blinn;
end;

function TXMLProfile_common_type_technique.Get_Extra: IXMLExtra_typeList;
begin
  Result := FExtra;
end;

{ TXMLProfile_common_type_technique_constant }

procedure TXMLProfile_common_type_technique_constant.AfterConstruction;
begin
  RegisterChildNode('emission', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('reflective', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('reflectivity', TXMLFx_common_float_or_param_type);
  RegisterChildNode('transparent', TXMLFx_common_transparent_type);
  RegisterChildNode('transparency', TXMLFx_common_float_or_param_type);
  RegisterChildNode('index_of_refraction', TXMLFx_common_float_or_param_type);
  inherited;
end;

function TXMLProfile_common_type_technique_constant.Get_Emission: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['emission'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_constant.Get_Reflective: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['reflective'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_constant.Get_Reflectivity: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['reflectivity'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_constant.Get_Transparent: IXMLFx_common_transparent_type;
begin
  Result := ChildNodes['transparent'] as IXMLFx_common_transparent_type;
end;

function TXMLProfile_common_type_technique_constant.Get_Transparency: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['transparency'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_constant.Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['index_of_refraction'] as IXMLFx_common_float_or_param_type;
end;

{ TXMLFx_common_color_or_texture_type }

procedure TXMLFx_common_color_or_texture_type.AfterConstruction;
begin
  RegisterChildNode('color', TXMLFx_common_color_or_texture_type_color);
  RegisterChildNode('param', TXMLFx_common_color_or_texture_type_param);
  RegisterChildNode('texture', TXMLFx_common_color_or_texture_type_texture);
  inherited;
end;

function TXMLFx_common_color_or_texture_type.Get_Color: IXMLFx_common_color_or_texture_type_color;
begin
  Result := ChildNodes['color'] as IXMLFx_common_color_or_texture_type_color;
end;

function TXMLFx_common_color_or_texture_type.Get_Param: IXMLFx_common_color_or_texture_type_param;
begin
  Result := ChildNodes['param'] as IXMLFx_common_color_or_texture_type_param;
end;

function TXMLFx_common_color_or_texture_type.Get_Texture: IXMLFx_common_color_or_texture_type_texture;
begin
  Result := ChildNodes['texture'] as IXMLFx_common_color_or_texture_type_texture;
end;

{ TXMLFx_common_color_or_texture_type_color }

function TXMLFx_common_color_or_texture_type_color.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLFx_common_color_or_texture_type_color.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

{ TXMLFx_common_color_or_texture_type_param }

function TXMLFx_common_color_or_texture_type_param.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLFx_common_color_or_texture_type_param.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLFx_common_color_or_texture_type_texture }

procedure TXMLFx_common_color_or_texture_type_texture.AfterConstruction;
begin
  RegisterChildNode('extra', TXMLExtra_type);
  ItemTag := 'extra';
  ItemInterface := IXMLExtra_type;
  inherited;
end;

function TXMLFx_common_color_or_texture_type_texture.Get_Texture: UnicodeString;
begin
  Result := AttributeNodes['texture'].Text;
end;

procedure TXMLFx_common_color_or_texture_type_texture.Set_Texture(Value: UnicodeString);
begin
  SetAttribute('texture', Value);
end;

function TXMLFx_common_color_or_texture_type_texture.Get_Texcoord: UnicodeString;
begin
  Result := AttributeNodes['texcoord'].Text;
end;

procedure TXMLFx_common_color_or_texture_type_texture.Set_Texcoord(Value: UnicodeString);
begin
  SetAttribute('texcoord', Value);
end;

function TXMLFx_common_color_or_texture_type_texture.Get_Extra(Index: Integer): IXMLExtra_type;
begin
  Result := List[Index] as IXMLExtra_type;
end;

function TXMLFx_common_color_or_texture_type_texture.Add: IXMLExtra_type;
begin
  Result := AddItem(-1) as IXMLExtra_type;
end;

function TXMLFx_common_color_or_texture_type_texture.Insert(const Index: Integer): IXMLExtra_type;
begin
  Result := AddItem(Index) as IXMLExtra_type;
end;

{ TXMLFx_common_float_or_param_type }

procedure TXMLFx_common_float_or_param_type.AfterConstruction;
begin
  RegisterChildNode('float', TXMLFx_common_float_or_param_type_float);
  RegisterChildNode('param', TXMLFx_common_float_or_param_type_param);
  inherited;
end;

function TXMLFx_common_float_or_param_type.Get_Float: IXMLFx_common_float_or_param_type_float;
begin
  Result := ChildNodes['float'] as IXMLFx_common_float_or_param_type_float;
end;

function TXMLFx_common_float_or_param_type.Get_Param: IXMLFx_common_float_or_param_type_param;
begin
  Result := ChildNodes['param'] as IXMLFx_common_float_or_param_type_param;
end;

{ TXMLFx_common_float_or_param_type_float }

function TXMLFx_common_float_or_param_type_float.Get_Sid: UnicodeString;
begin
  Result := AttributeNodes['sid'].Text;
end;

procedure TXMLFx_common_float_or_param_type_float.Set_Sid(Value: UnicodeString);
begin
  SetAttribute('sid', Value);
end;

{ TXMLFx_common_float_or_param_type_param }

function TXMLFx_common_float_or_param_type_param.Get_Ref: UnicodeString;
begin
  Result := AttributeNodes['ref'].Text;
end;

procedure TXMLFx_common_float_or_param_type_param.Set_Ref(Value: UnicodeString);
begin
  SetAttribute('ref', Value);
end;

{ TXMLFx_common_transparent_type }

function TXMLFx_common_transparent_type.Get_Opaque: UnicodeString;
begin
  Result := AttributeNodes['opaque'].Text;
end;

procedure TXMLFx_common_transparent_type.Set_Opaque(Value: UnicodeString);
begin
  SetAttribute('opaque', Value);
end;

{ TXMLProfile_common_type_technique_lambert }

procedure TXMLProfile_common_type_technique_lambert.AfterConstruction;
begin
  RegisterChildNode('emission', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('ambient', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('diffuse', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('reflective', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('reflectivity', TXMLFx_common_float_or_param_type);
  RegisterChildNode('transparent', TXMLFx_common_transparent_type);
  RegisterChildNode('transparency', TXMLFx_common_float_or_param_type);
  RegisterChildNode('index_of_refraction', TXMLFx_common_float_or_param_type);
  inherited;
end;

function TXMLProfile_common_type_technique_lambert.Get_Emission: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['emission'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Ambient: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['ambient'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Diffuse: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['diffuse'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Reflective: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['reflective'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Reflectivity: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['reflectivity'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Transparent: IXMLFx_common_transparent_type;
begin
  Result := ChildNodes['transparent'] as IXMLFx_common_transparent_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Transparency: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['transparency'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_lambert.Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['index_of_refraction'] as IXMLFx_common_float_or_param_type;
end;

{ TXMLProfile_common_type_technique_phong }

procedure TXMLProfile_common_type_technique_phong.AfterConstruction;
begin
  RegisterChildNode('emission', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('ambient', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('diffuse', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('specular', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('shininess', TXMLFx_common_float_or_param_type);
  RegisterChildNode('reflective', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('reflectivity', TXMLFx_common_float_or_param_type);
  RegisterChildNode('transparent', TXMLFx_common_transparent_type);
  RegisterChildNode('transparency', TXMLFx_common_float_or_param_type);
  RegisterChildNode('index_of_refraction', TXMLFx_common_float_or_param_type);
  inherited;
end;

function TXMLProfile_common_type_technique_phong.Get_Emission: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['emission'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Ambient: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['ambient'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Diffuse: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['diffuse'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Specular: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['specular'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Shininess: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['shininess'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Reflective: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['reflective'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Reflectivity: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['reflectivity'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Transparent: IXMLFx_common_transparent_type;
begin
  Result := ChildNodes['transparent'] as IXMLFx_common_transparent_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Transparency: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['transparency'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_phong.Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['index_of_refraction'] as IXMLFx_common_float_or_param_type;
end;

{ TXMLProfile_common_type_technique_blinn }

procedure TXMLProfile_common_type_technique_blinn.AfterConstruction;
begin
  RegisterChildNode('emission', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('ambient', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('diffuse', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('specular', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('shininess', TXMLFx_common_float_or_param_type);
  RegisterChildNode('reflective', TXMLFx_common_color_or_texture_type);
  RegisterChildNode('reflectivity', TXMLFx_common_float_or_param_type);
  RegisterChildNode('transparent', TXMLFx_common_transparent_type);
  RegisterChildNode('transparency', TXMLFx_common_float_or_param_type);
  RegisterChildNode('index_of_refraction', TXMLFx_common_float_or_param_type);
  inherited;
end;

function TXMLProfile_common_type_technique_blinn.Get_Emission: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['emission'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Ambient: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['ambient'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Diffuse: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['diffuse'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Specular: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['specular'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Shininess: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['shininess'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Reflective: IXMLFx_common_color_or_texture_type;
begin
  Result := ChildNodes['reflective'] as IXMLFx_common_color_or_texture_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Reflectivity: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['reflectivity'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Transparent: IXMLFx_common_transparent_type;
begin
  Result := ChildNodes['transparent'] as IXMLFx_common_transparent_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Transparency: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['transparency'] as IXMLFx_common_float_or_param_type;
end;

function TXMLProfile_common_type_technique_blinn.Get_Index_of_refraction: IXMLFx_common_float_or_param_type;
begin
  Result := ChildNodes['index_of_refraction'] as IXMLFx_common_float_or_param_type;
end;


end.
