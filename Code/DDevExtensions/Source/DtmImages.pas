{******************************************************************************}
{*                                                                            *}
{* DDevExtensions                                                             *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DtmImages;

{$I DelphiExtension.inc}

interface

uses
  SysUtils, Classes, ImgList, Controls, Forms;

type
  TDataModuleImages = class(TDataModule)
    imlIcons: TImageList;
    imlFilter: TImageList;
    imlModules: TImageList;
    imlApplications: TImageList;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  DataModuleImages: TDataModuleImages;

const
  imgModuleUnit = 0;
  imgModuleForm = 1;
  imgModuleFrame = 2;
  imgModuleDataModule = 3;
  imgModuleInheritForm = 4;
  imgModuleBinary = 5;

implementation

{$R *.dfm}

end.
