{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

const
{
 * POSIX scheduling policies
}
  SCHED_OTHER = 0;
  {$EXTERNALSYM SCHED_OTHER}
  SCHED_FIFO = 1;
  {$EXTERNALSYM SCHED_FIFO}
  SCHED_RR = 2;
  {$EXTERNALSYM SCHED_RR}

type
  sched_param = record
    sched_priority: Int32;
  end;
  {$EXTERNALSYM sched_param}
  Psched_param = ^sched_param;
