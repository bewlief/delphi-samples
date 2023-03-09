{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2013-2022 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Androidapi.JNI.PlayServices.Tasks;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.JavaTypes;

type
// ===== Forward declarations =====

  JCancellationToken = interface;//com.google.android.gms.tasks.CancellationToken
  JCancellationTokenSource = interface;//com.google.android.gms.tasks.CancellationTokenSource
  JContinuation = interface;//com.google.android.gms.tasks.Continuation
  JDuplicateTaskCompletionException = interface;//com.google.android.gms.tasks.DuplicateTaskCompletionException
  JOnCanceledListener = interface;//com.google.android.gms.tasks.OnCanceledListener
  JOnCompleteListener = interface;//com.google.android.gms.tasks.OnCompleteListener
  JOnFailureListener = interface;//com.google.android.gms.tasks.OnFailureListener
  JOnSuccessListener = interface;//com.google.android.gms.tasks.OnSuccessListener
  JOnTokenCanceledListener = interface;//com.google.android.gms.tasks.OnTokenCanceledListener
  JRuntimeExecutionException = interface;//com.google.android.gms.tasks.RuntimeExecutionException
  JSuccessContinuation = interface;//com.google.android.gms.tasks.SuccessContinuation
  JTask = interface;//com.google.android.gms.tasks.Task
  JTaskCompletionSource = interface;//com.google.android.gms.tasks.TaskCompletionSource
  JTaskExecutors = interface;//com.google.android.gms.tasks.TaskExecutors
  //JTaskExecutors_zza = interface;//com.google.android.gms.tasks.TaskExecutors$zza
  JTasks = interface;//com.google.android.gms.tasks.Tasks
  //JTasks_zza = interface;//com.google.android.gms.tasks.Tasks$zza
  //JTasks_zzb = interface;//com.google.android.gms.tasks.Tasks$zzb
  //JTasks_zzc = interface;//com.google.android.gms.tasks.Tasks$zzc
  //Jgms_tasks_zza = interface;//com.google.android.gms.tasks.zza
  //Jtasks_zzaa = interface;//com.google.android.gms.tasks.zzaa
  //Jgms_tasks_zzb = interface;//com.google.android.gms.tasks.zzb
  //Jtasks_zzr = interface;//com.google.android.gms.tasks.zzr
  //Jgms_tasks_zzc = interface;//com.google.android.gms.tasks.zzc
  //Jtasks_zzd = interface;//com.google.android.gms.tasks.zzd
  //Jtasks_zze = interface;//com.google.android.gms.tasks.zze
  //Jtasks_zzf = interface;//com.google.android.gms.tasks.zzf
  //Jtasks_zzg = interface;//com.google.android.gms.tasks.zzg
  //Jtasks_zzh = interface;//com.google.android.gms.tasks.zzh
  //Jtasks_zzi = interface;//com.google.android.gms.tasks.zzi
  //Jtasks_zzj = interface;//com.google.android.gms.tasks.zzj
  //Jtasks_zzk = interface;//com.google.android.gms.tasks.zzk
  //Jtasks_zzl = interface;//com.google.android.gms.tasks.zzl
  //Jtasks_zzm = interface;//com.google.android.gms.tasks.zzm
  //Jtasks_zzn = interface;//com.google.android.gms.tasks.zzn
  //Jtasks_zzo = interface;//com.google.android.gms.tasks.zzo
  //Jtasks_zzp = interface;//com.google.android.gms.tasks.zzp
  //Jtasks_zzq = interface;//com.google.android.gms.tasks.zzq
  //Jtasks_zzs = interface;//com.google.android.gms.tasks.zzs
  //Jtasks_zzt = interface;//com.google.android.gms.tasks.zzt
  //Jtasks_zzu = interface;//com.google.android.gms.tasks.zzu
  //Jtasks_zzu_zza = interface;//com.google.android.gms.tasks.zzu$zza
  //Jtasks_zzv = interface;//com.google.android.gms.tasks.zzv
  //Jtasks_zzw = interface;//com.google.android.gms.tasks.zzw
  //Jtasks_zzx = interface;//com.google.android.gms.tasks.zzx
  //Jtasks_zzy = interface;//com.google.android.gms.tasks.zzy
  //Jtasks_zzz = interface;//com.google.android.gms.tasks.zzz

// ===== Interface declarations =====

  JCancellationTokenClass = interface(JObjectClass)
    ['{54992047-8D32-483B-9F0E-014DA5856366}']
    {class} function init: JCancellationToken; cdecl;
  end;

  [JavaSignature('com/google/android/gms/tasks/CancellationToken')]
  JCancellationToken = interface(JObject)
    ['{07EB8B2D-D35A-4EE8-95CE-DE6ABF6F25F1}']
    function isCancellationRequested: Boolean; cdecl;
    function onCanceledRequested(onTokenCanceledListener: JOnTokenCanceledListener): JCancellationToken; cdecl;
  end;
  TJCancellationToken = class(TJavaGenericImport<JCancellationTokenClass, JCancellationToken>) end;

  JCancellationTokenSourceClass = interface(JObjectClass)
    ['{9766E52C-D42C-4A6B-A267-FE1FC09CFA24}']
    {class} function init: JCancellationTokenSource; cdecl;
  end;

  [JavaSignature('com/google/android/gms/tasks/CancellationTokenSource')]
  JCancellationTokenSource = interface(JObject)
    ['{778D0256-1758-487C-BC4E-0A5707F4C964}']
    procedure cancel; cdecl;
    function getToken: JCancellationToken; cdecl;
  end;
  TJCancellationTokenSource = class(TJavaGenericImport<JCancellationTokenSourceClass, JCancellationTokenSource>) end;

  JContinuationClass = interface(IJavaClass)
    ['{FCE10375-D6B8-4A1B-88E3-C3A56193AF87}']
  end;

  [JavaSignature('com/google/android/gms/tasks/Continuation')]
  JContinuation = interface(IJavaInstance)
    ['{969CD9CE-2481-4E01-B998-B5E37D1B34A4}']
    //function &then(task: JTask): J; cdecl;
  end;
  TJContinuation = class(TJavaGenericImport<JContinuationClass, JContinuation>) end;

  JDuplicateTaskCompletionExceptionClass = interface(JIllegalStateExceptionClass)
    ['{AEAECA89-3B6B-48EE-A110-67F375BF5C60}']
    {class} function &of(task: JTask): JIllegalStateException; cdecl;
  end;

  [JavaSignature('com/google/android/gms/tasks/DuplicateTaskCompletionException')]
  JDuplicateTaskCompletionException = interface(JIllegalStateException)
    ['{7DAE5FC4-B539-4F60-A183-2EAFAAD20540}']
  end;
  TJDuplicateTaskCompletionException = class(TJavaGenericImport<JDuplicateTaskCompletionExceptionClass, JDuplicateTaskCompletionException>) end;

  JOnCanceledListenerClass = interface(IJavaClass)
    ['{004542B2-41C0-4976-89EB-71272CB46F76}']
  end;

  [JavaSignature('com/google/android/gms/tasks/OnCanceledListener')]
  JOnCanceledListener = interface(IJavaInstance)
    ['{A43992EC-6A69-4DC7-A222-0E812661C382}']
    procedure onCanceled; cdecl;
  end;
  TJOnCanceledListener = class(TJavaGenericImport<JOnCanceledListenerClass, JOnCanceledListener>) end;

  JOnCompleteListenerClass = interface(IJavaClass)
    ['{70B28546-8DC5-44A9-9348-D016D9CFC152}']
  end;

  [JavaSignature('com/google/android/gms/tasks/OnCompleteListener')]
  JOnCompleteListener = interface(IJavaInstance)
    ['{6EA0E0AA-3D47-4DCF-AE5C-6D861C4AA42F}']
    procedure onComplete(task: JTask); cdecl;
  end;
  TJOnCompleteListener = class(TJavaGenericImport<JOnCompleteListenerClass, JOnCompleteListener>) end;

  JOnFailureListenerClass = interface(IJavaClass)
    ['{1BC53B31-7582-4515-9159-1BFDEA41F529}']
  end;

  [JavaSignature('com/google/android/gms/tasks/OnFailureListener')]
  JOnFailureListener = interface(IJavaInstance)
    ['{37C18E4E-53EE-4A61-B6CD-C6D57D6FA134}']
    procedure onFailure(exception: JException); cdecl;
  end;
  TJOnFailureListener = class(TJavaGenericImport<JOnFailureListenerClass, JOnFailureListener>) end;

  JOnSuccessListenerClass = interface(IJavaClass)
    ['{FC00DCC1-A036-4F01-856A-6153512B1E8B}']
  end;

  [JavaSignature('com/google/android/gms/tasks/OnSuccessListener')]
  JOnSuccessListener = interface(IJavaInstance)
    ['{DD9283EA-F418-4BAA-A8A8-34309E4245F8}']
    procedure onSuccess(tResult: JObject); cdecl;
  end;
  TJOnSuccessListener = class(TJavaGenericImport<JOnSuccessListenerClass, JOnSuccessListener>) end;

  JOnTokenCanceledListenerClass = interface(IJavaClass)
    ['{E47F998C-DEDB-4A13-82FF-F8294E5706AA}']
  end;

  [JavaSignature('com/google/android/gms/tasks/OnTokenCanceledListener')]
  JOnTokenCanceledListener = interface(IJavaInstance)
    ['{E48D9AF3-F505-4411-8D28-0BEB7F49F0F2}']
    procedure onCanceled; cdecl;
  end;
  TJOnTokenCanceledListener = class(TJavaGenericImport<JOnTokenCanceledListenerClass, JOnTokenCanceledListener>) end;

  JRuntimeExecutionExceptionClass = interface(JRuntimeExceptionClass)
    ['{494546B8-2948-4530-8CEE-E68CDD528442}']
    {class} function init(throwable: JThrowable): JRuntimeExecutionException; cdecl;
  end;

  [JavaSignature('com/google/android/gms/tasks/RuntimeExecutionException')]
  JRuntimeExecutionException = interface(JRuntimeException)
    ['{118E2731-9CFA-436D-A263-E84BB7D93C96}']
  end;
  TJRuntimeExecutionException = class(TJavaGenericImport<JRuntimeExecutionExceptionClass, JRuntimeExecutionException>) end;

  JSuccessContinuationClass = interface(IJavaClass)
    ['{170D3825-22A9-4642-A79B-D9C56D188896}']
  end;

  [JavaSignature('com/google/android/gms/tasks/SuccessContinuation')]
  JSuccessContinuation = interface(IJavaInstance)
    ['{7595D13B-191F-426D-8C07-D0CC669AB156}']
    function &then(tResult: JObject): JTask; cdecl;
  end;
  TJSuccessContinuation = class(TJavaGenericImport<JSuccessContinuationClass, JSuccessContinuation>) end;

  JTaskClass = interface(JObjectClass)
    ['{E9F05104-CB8A-4155-B89D-DF06F7CAA0C2}']
    {class} function init: JTask; cdecl;
  end;

  [JavaSignature('com/google/android/gms/tasks/Task')]
  JTask = interface(JObject)
    ['{9C0C585F-1DD9-4437-A5EF-A55673956D11}']
    function addOnCanceledListener(onCanceledListener: JOnCanceledListener): JTask; cdecl; overload;
    function addOnCanceledListener(executor: JExecutor; onCanceledListener: JOnCanceledListener): JTask; cdecl; overload;
    function addOnCanceledListener(activity: JActivity; onCanceledListener: JOnCanceledListener): JTask; cdecl; overload;
    function addOnCompleteListener(onCompleteListener: JOnCompleteListener): JTask; cdecl; overload;
    function addOnCompleteListener(executor: JExecutor; onCompleteListener: JOnCompleteListener): JTask; cdecl; overload;
    function addOnCompleteListener(activity: JActivity; onCompleteListener: JOnCompleteListener): JTask; cdecl; overload;
    function addOnFailureListener(onFailureListener: JOnFailureListener): JTask; cdecl; overload;
    function addOnFailureListener(executor: JExecutor; onFailureListener: JOnFailureListener): JTask; cdecl; overload;
    function addOnFailureListener(activity: JActivity; onFailureListener: JOnFailureListener): JTask; cdecl; overload;
    function addOnSuccessListener(onSuccessListener: JOnSuccessListener): JTask; cdecl; overload;
    function addOnSuccessListener(activity: JActivity; onSuccessListener: JOnSuccessListener): JTask; cdecl; overload;
    function addOnSuccessListener(executor: JExecutor; onSuccessListener: JOnSuccessListener): JTask; cdecl; overload;
    function continueWith(continuation: JContinuation): JTask; cdecl; overload;
    function continueWith(executor: JExecutor; continuation: JContinuation): JTask; cdecl; overload;
    function continueWithTask(continuation: JContinuation): JTask; cdecl; overload;
    function continueWithTask(executor: JExecutor; continuation: JContinuation): JTask; cdecl; overload;
    function getException: JException; cdecl;
    function getResult: JObject; cdecl; overload;
    function getResult(class_: Jlang_Class): JObject; cdecl; overload;
    function isCanceled: Boolean; cdecl;
    function isComplete: Boolean; cdecl;
    function isSuccessful: Boolean; cdecl;
    function onSuccessTask(successContinuation: JSuccessContinuation): JTask; cdecl; overload;
    function onSuccessTask(executor: JExecutor; successContinuation: JSuccessContinuation): JTask; cdecl; overload;
  end;
  TJTask = class(TJavaGenericImport<JTaskClass, JTask>) end;

  JTaskCompletionSourceClass = interface(JObjectClass)
    ['{0830DA0D-0EFD-4C15-B32C-7F104132BF87}']
    {class} function init: JTaskCompletionSource; cdecl; overload;
    {class} function init(cancellationToken: JCancellationToken): JTaskCompletionSource; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/tasks/TaskCompletionSource')]
  JTaskCompletionSource = interface(JObject)
    ['{3124BB2B-37AA-4A27-89C8-2F66CC1791E4}']
    function getTask: JTask; cdecl;
    procedure setException(exception: JException); cdecl;
    procedure setResult(tResult: JObject); cdecl;
    function trySetException(exception: JException): Boolean; cdecl;
    function trySetResult(tResult: JObject): Boolean; cdecl;
  end;
  TJTaskCompletionSource = class(TJavaGenericImport<JTaskCompletionSourceClass, JTaskCompletionSource>) end;

  JTaskExecutorsClass = interface(JObjectClass)
    ['{1AB0DFE3-D64B-4217-8A2B-8A3547C87EC0}']
    {class} function _GetMAIN_THREAD: JExecutor; cdecl;
    {class} property MAIN_THREAD: JExecutor read _GetMAIN_THREAD;
  end;

  [JavaSignature('com/google/android/gms/tasks/TaskExecutors')]
  JTaskExecutors = interface(JObject)
    ['{F0DCBEA3-9101-4A0E-BB47-8FF8D41A5319}']
  end;
  TJTaskExecutors = class(TJavaGenericImport<JTaskExecutorsClass, JTaskExecutors>) end;

  // com.google.android.gms.tasks.TaskExecutors$zza
  JTasksClass = interface(JObjectClass)
    ['{BC74FDB7-14AF-4A19-B9DE-0B6F594A166F}']
    {class} //function await(task: JTask): J; cdecl; overload;
    {class} //function await(task: JTask; l: Int64; timeUnit: JTimeUnit): J; cdecl; overload;
    {class} function call(callable: JCallable): JTask; cdecl; overload;
    {class} function call(executor: JExecutor; callable: JCallable): JTask; cdecl; overload;
    {class} function forCanceled: JTask; cdecl;
    {class} function forException(exception: JException): JTask; cdecl;
    {class} function forResult(tResult: JObject): JTask; cdecl;
    {class} function whenAll(collection: JCollection): JTask; cdecl; overload;
    {class} function whenAllComplete(collection: JCollection): JTask; cdecl; overload;
    {class} function whenAllSuccess(collection: JCollection): JTask; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/tasks/Tasks')]
  JTasks = interface(JObject)
    ['{14AAFE98-2354-4252-A90D-A0D31A7366BB}']
  end;
  TJTasks = class(TJavaGenericImport<JTasksClass, JTasks>) end;

  // com.google.android.gms.tasks.Tasks$zza
  // com.google.android.gms.tasks.Tasks$zzb
  // com.google.android.gms.tasks.Tasks$zzc
  // com.google.android.gms.tasks.zza
  // com.google.android.gms.tasks.zzaa
  // com.google.android.gms.tasks.zzb
  // com.google.android.gms.tasks.zzr
  // com.google.android.gms.tasks.zzc
  // com.google.android.gms.tasks.zzd
  // com.google.android.gms.tasks.zze
  // com.google.android.gms.tasks.zzf
  // com.google.android.gms.tasks.zzg
  // com.google.android.gms.tasks.zzh
  // com.google.android.gms.tasks.zzi
  // com.google.android.gms.tasks.zzj
  // com.google.android.gms.tasks.zzk
  // com.google.android.gms.tasks.zzl
  // com.google.android.gms.tasks.zzm
  // com.google.android.gms.tasks.zzn
  // com.google.android.gms.tasks.zzo
  // com.google.android.gms.tasks.zzp
  // com.google.android.gms.tasks.zzq
  // com.google.android.gms.tasks.zzs
  // com.google.android.gms.tasks.zzt
  // com.google.android.gms.tasks.zzu
  // com.google.android.gms.tasks.zzu$zza
  // com.google.android.gms.tasks.zzv
  // com.google.android.gms.tasks.zzw
  // com.google.android.gms.tasks.zzx
  // com.google.android.gms.tasks.zzy
  // com.google.android.gms.tasks.zzz
implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JCancellationToken', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JCancellationToken));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JCancellationTokenSource', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JCancellationTokenSource));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JContinuation', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JContinuation));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JDuplicateTaskCompletionException', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JDuplicateTaskCompletionException));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JOnCanceledListener', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JOnCanceledListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JOnCompleteListener', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JOnCompleteListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JOnFailureListener', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JOnFailureListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JOnSuccessListener', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JOnSuccessListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JOnTokenCanceledListener', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JOnTokenCanceledListener));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JRuntimeExecutionException', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JRuntimeExecutionException));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JSuccessContinuation', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JSuccessContinuation));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTask', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTask));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTaskCompletionSource', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTaskCompletionSource));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTaskExecutors', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTaskExecutors));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTaskExecutors_zza', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTaskExecutors_zza));
  TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTasks', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTasks));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTasks_zza', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTasks_zza));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTasks_zzb', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTasks_zzb));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.JTasks_zzc', TypeInfo(Androidapi.JNI.PlayServices.Tasks.JTasks_zzc));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jgms_tasks_zza', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jgms_tasks_zza));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzaa', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzaa));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jgms_tasks_zzb', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jgms_tasks_zzb));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzr', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzr));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jgms_tasks_zzc', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jgms_tasks_zzc));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzd', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzd));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zze', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zze));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzf', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzf));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzg', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzg));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzh', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzh));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzi', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzi));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzj', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzj));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzk', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzk));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzl', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzl));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzm', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzm));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzn', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzn));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzo', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzo));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzp', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzp));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzq', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzq));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzs', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzs));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzt', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzt));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzu', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzu));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzu_zza', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzu_zza));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzv', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzv));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzw', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzw));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzx', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzx));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzy', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzy));
  //TRegTypes.RegisterType('Androidapi.JNI.PlayServices.Tasks.Jtasks_zzz', TypeInfo(Androidapi.JNI.PlayServices.Tasks.Jtasks_zzz));
end;

initialization
  RegisterTypes;
end.


