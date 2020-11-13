{******************************************************************************}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit SimpleXmlImport;

interface

uses
  SimpleXmlIntf, SimpleXmlDoc;

var
  LoadXmlDocument: function(const Filename: string): IXmlDocument
    = SimpleXmlDoc.LoadXmlDocument;
  NewXmlDocument: function(const Version: string = ''): IXmlDocument
    = SimpleXmlDoc.NewXmlDocument;

{function LoadXmlDocument(const Filename: string): IXmlDocument;
  external 'DDevExXml.dll';
function NewXmlDocument(const Version: string = ''): IXmlDocument;
  external 'DDevExXml.dll';}

implementation

end.
