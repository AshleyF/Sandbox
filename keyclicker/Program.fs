open System
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Windows.Media
open System.Windows.Forms

[<Literal>]
let WH_KEYBOARD_LL = 13

[<StructLayout(LayoutKind.Sequential)>]
type KBDLLHOOKSTRUCT =
    val vkCode      : uint32
    val scanCode    : uint32
    val flags       : uint32
    val time        : uint32
    val dwExtraInfo : nativeint

type LowLevelKeyboardProc = delegate of int * nativeint * KBDLLHOOKSTRUCT -> nativeint

[<DllImport("kernel32.dll")>]
extern nativeint GetModuleHandle(string lpModuleName)

[<DllImport("user32.dll")>]
extern bool UnhookWindowsHookEx(nativeint hhk)

[<DllImport("user32.dll")>]
extern nativeint SetWindowsHookEx(int idhook, LowLevelKeyboardProc proc, nativeint hMod, uint32 threadId)

[<DllImport("user32.dll")>]
extern nativeint CallNextHookEx(IntPtr hhk, int nCode, IntPtr wParam, KBDLLHOOKSTRUCT lParam)

let SetHook (proc: LowLevelKeyboardProc) =
    use curProc = Process.GetCurrentProcess ()
    use curMod = curProc.MainModule
    SetWindowsHookEx(WH_KEYBOARD_LL, proc, GetModuleHandle(curMod.ModuleName), 0u)

type App(handler) as x =
    inherit ApplicationContext()
    let rec callback (code : int) (wparam : nativeint) (lparam : KBDLLHOOKSTRUCT) : nativeint =
        match handler code wparam lparam with
        | Some keys ->
            if String.length keys > 0 then SendKeys.Send keys
            nativeint 1
        | None -> CallNextHookEx(hook, code, wparam, lparam)
    and proc = new LowLevelKeyboardProc(callback)
    and hook = SetHook proc
    override x.ExitThreadCore() =
        UnhookWindowsHookEx(hook) |> ignore
        base.ExitThreadCore()

let player = new MediaPlayer();
player.Open(new System.Uri(Path.Combine(Environment.CurrentDirectory, "Click.wav")));

Console.WriteLine("Leave this running in the background, playing keyboard clicks as you type in any forground app.")

Application.Run(new App(fun code wparam lparam ->
    let keydown = lparam.flags &&& 0b10000000u = 0u
    if keydown then
        player.Play()
        player.Position <- TimeSpan.Zero
    None))
