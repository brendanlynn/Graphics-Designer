Module Program
    Private Declare Auto Function ShowWindow Lib "user32.dll" (ByVal hWnd As IntPtr, ByVal nCmdShow As Integer) As Boolean
    Private Declare Auto Function GetConsoleWindow Lib "kernel32.dll" () As IntPtr
    Public outputArgs As String()
    Sub Main(args As String())
        ShowWindow(GetConsoleWindow(), 0)
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Dim MainForm As New Form1()
        outputArgs = args
        Application.Run(MainForm)
    End Sub
End Module
