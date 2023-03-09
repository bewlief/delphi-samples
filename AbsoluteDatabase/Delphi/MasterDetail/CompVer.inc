{$UNDEF D4H}
{$UNDEF C4H}
{$UNDEF D5H}
{$UNDEF C5H}
{$UNDEF D6H}
{$UNDEF C6H}
{$UNDEF D7H}

{$IFNDEF VER120}
  {$IFNDEF VER125}
    {$IFNDEF VER130}
      {$IFNDEF VER135}
       {$IFNDEF VER140}
        {$IFNDEF VER145}
          {$DEFINE D7H}
          {$DEFINE C6H}
        {$ELSE}
          {$DEFINE C6H}
        {$ENDIF}
       {$ENDIF}

       {$DEFINE D5H}
       {$DEFINE C5H}
       {$DEFINE D6H}
      {$ELSE}
        {$DEFINE D5H}
        {$DEFINE C5H}
      {$ENDIF}
    {$ELSE}
      {$DEFINE D5H}
    {$ENDIF}
  {$ELSE}
    {$DEFINE D4H}
    {$DEFINE C4H}
  {$ENDIF}
{$ELSE}
  {$DEFINE D4H}
{$ENDIF}
