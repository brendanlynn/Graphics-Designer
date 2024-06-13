Public Class Form1
    Dim fileLocation As String = ""
    Private Sub RichTextBox1_TextChanged(sender As Object, e As EventArgs) Handles RichTextBox1.TextChanged
        Dim text As String = RichTextBox1.Text
        If text.Length > 0 Then
            Dim newText As String = ""
            Dim locked As Boolean = False
            For i As Integer = 0 To text.Length - 1
                If text(i) = """"c Then
                    locked = Not locked
                    newText &= text(i)
                Else
                    If locked Then
                        newText &= text(i)
                    Else
                        newText &= Char.ToLower(text(i))
                    End If
                End If
            Next
            locked = False
            text = newText
            newText = ""
            Dim currentStart As Integer = RichTextBox1.SelectionStart
            Dim currentLength As Integer = RichTextBox1.SelectionLength
            For i As Integer = 0 To text.Length - 2
                If text(i) = """"c Then
                    locked = Not locked
                    newText &= text(i)
                ElseIf (Not (text(i) = " "c And text(i + 1) = " ")) Or locked Then
                    newText &= text(i)
                Else
                    If i < currentStart Then
                        currentStart -= 1
                    ElseIf i < currentStart + currentLength Then
                        currentLength -= 1
                    End If
                End If
            Next
            newText &= text(text.Length - 1)
            RichTextBox1.Text = newText
            RichTextBox1.SelectionStart = currentStart
            RichTextBox1.SelectionLength = currentLength
        End If
        Check()
    End Sub
    Private Sub Check()
        Dim text As String = RichTextBox1.Text
        If text.Length = 0 Then
            PictureBox1.Image = Nothing
        Else
            Dim widthString As String = TextBox1.Text
            Dim heightString As String = TextBox2.Text
            If widthString.Length <> 0 And heightString.Length <> 0 Then
                Dim width As Integer = Integer.Parse(widthString)
                Dim height As Integer = Integer.Parse(heightString)
                If width <> 0 And height <> 0 Then PictureBox1.Image = Calculate(text, width, height)
            End If
        End If
    End Sub
    Private ReadOnly numbers As Char() = {"0"c, "1"c, "2"c, "3"c, "4"c, "5"c, "6"c, "7"c, "8"c, "9"c}
    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        If TextBox1.Text.Length <> 0 Then
            Dim stringNumbers As String = TextBox1.Text
            Dim selectionStart As Integer = TextBox1.SelectionStart
            Dim selectionLength As Integer = TextBox1.SelectionLength
            Dim newStringNumbers As String = ""
            For i As UInt64 = 0 To stringNumbers.Length - 1
                If numbers.Contains(stringNumbers(i)) Then
                    newStringNumbers &= stringNumbers(i)
                ElseIf i < selectionStart Then
                    selectionStart -= 1
                ElseIf i - selectionStart < selectionLength Then
                    selectionLength -= 1
                End If
            Next
            While newStringNumbers.StartsWith("0"c)
                newStringNumbers = newStringNumbers.Substring(1)
            End While
            If newStringNumbers.Length = 0 Then newStringNumbers = "0"
            TextBox1.SuspendLayout()
            TextBox1.Text = newStringNumbers
            TextBox1.SelectionStart = selectionStart
            TextBox1.SelectionLength = selectionLength
            TextBox1.ResumeLayout(True)
            Check()
        End If
    End Sub
    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles TextBox2.TextChanged
        If TextBox2.Text.Length <> 0 Then
            Dim stringNumbers As String = TextBox2.Text
            Dim selectionStart As Integer = TextBox2.SelectionStart
            Dim selectionLength As Integer = TextBox2.SelectionLength
            Dim newStringNumbers As String = ""
            For i As UInt64 = 0 To stringNumbers.Length - 1
                If numbers.Contains(stringNumbers(i)) Then
                    newStringNumbers &= stringNumbers(i)
                ElseIf i < selectionStart Then
                    selectionStart -= 1
                ElseIf i - selectionStart < selectionLength Then
                    selectionLength -= 1
                End If
            Next
            While newStringNumbers.StartsWith("0"c)
                newStringNumbers = newStringNumbers.Substring(1)
            End While
            If newStringNumbers.Length = 0 Then newStringNumbers = "0"
            TextBox2.SuspendLayout()
            TextBox2.Text = newStringNumbers
            TextBox2.SelectionStart = selectionStart
            TextBox2.SelectionLength = selectionLength
            TextBox2.ResumeLayout(True)
            Check()
        End If
    End Sub
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If TextBox1.Text.Length = 0 Or TextBox2.Text.Length = 0 Then Return
        If fileLocation <> "" Then
            IO.File.WriteAllText(fileLocation, Serialize())
        Else
            SaveFileDialog1.ShowDialog()
        End If
    End Sub
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If TextBox1.Text.Length = 0 Or TextBox2.Text.Length = 0 Then Return
        SaveFileDialog1.ShowDialog()
    End Sub
    Private Sub SaveFileDialog1_FileOk(sender As Object, e As System.ComponentModel.CancelEventArgs) Handles SaveFileDialog1.FileOk
        Dim fileName As String = SaveFileDialog1.FileName
        If fileName.EndsWith(".grdd") Then
            IO.File.WriteAllText(fileName, Serialize())
            fileLocation = fileName
        Else
            PictureBox1.Image.Save(fileName)
        End If
    End Sub
    Private Function Serialize()
        Return TextBox1.Text & "," & TextBox2.Text & "," & RichTextBox1.Text
    End Function
    Private Function Calculate(commands As String, width As Integer, height As Integer) As Bitmap
        Dim workingBitmap As New Bitmap(width, height)
        Dim graphic As Drawing.Graphics = Graphics.FromImage(workingBitmap)
        Dim defs As New Dictionary(Of String, String())()
        commands = commands.Replace(vbCrLf, " ").Replace(vbCr, " ").Replace(vbLf, " ")
        While commands.Contains("  ")
            commands = commands.Replace("  ", " ")
        End While
        Dim commandLines As String() = CutLines(commands)
        Calculate(commandLines, graphic, defs, New Rectangle(0, 0, width, height))
        Return workingBitmap
    End Function
    Private Sub Calculate(commandLines As String(), ByRef graphic As Graphics, defs As Dictionary(Of String, String()), region As Rectangle)
        Dim stack As UInteger = 0
        Dim currentKey As String
        Dim currentList As New List(Of String)()
        Dim executingCommands As New List(Of String)()
        For Each command As String In commandLines
            command = command.Trim()
            If command.Length = 0 Then Continue For
            If stack > 0 Then
                currentList.Add(command)
            End If
            Dim commandSections As String() = Cut(command.Substring(0, command.Length - 1).Trim())
            If commandSections.Length = 2 AndAlso commandSections(0) = "def" AndAlso (Not defs.ContainsKey(commandSections(1))) AndAlso command.EndsWith("{"c) Then
                stack += 1
                If stack = 1 Then currentKey = commandSections(1)
            End If
            If stack = 0 Then executingCommands.Add(command)
            If command.EndsWith("}"c) Then
                If stack = 1 Then
                    defs(currentKey) = currentList.ToArray()
                    currentList.Clear()
                End If
                stack = Math.Max(0, stack - 1)
            End If
        Next
        For Each command As String In executingCommands
            If command.Length <> 0 Then
                ExecuteCommand(command.Substring(0, command.Length - 1), graphic, defs, region)
            End If
        Next
    End Sub
    Private Function Cut(command As String) As String()
        Dim commandSectionsList As New List(Of String)() From {""}
        Dim index As UInt64 = 0
        Dim locked As Boolean = False
        For Each i As Char In command
            If i = " "c And (Not locked) Then
                index += 1
                commandSectionsList.Add("")
            ElseIf i = """"c Then
                locked = Not locked
            Else
                commandSectionsList(index) &= i
            End If
        Next
        Return commandSectionsList.ToArray()
    End Function
    Private Function CutLines(commands As String) As String()
        If commands.Length = 0 Then Return {}
        Dim commandLinesList As New List(Of String)()
        Dim last As ULong = 0
        For i As ULong = 0 To commands.Length - 1
            If commands(i) = ";"c Or commands(i) = "{"c Or commands(i) = "}"c Then
                commandLinesList.Add(commands.Substring(last, i - last + 1))
                last = i + 1
            End If
        Next
        commandLinesList.Add(commands.Substring(last))
        Return commandLinesList.ToArray()
    End Function
    Private Sub ExecuteCommand(command As String, ByRef graphic As Graphics, defs As Dictionary(Of String, String()), region As Rectangle)
        command = command.Trim()
        Dim commandSections As String() = Cut(command)
        Select Case commandSections(0)
            Case "draw"
                If commandSections.Length <> 6 Then Return
                If defs.ContainsKey(commandSections(1)) Then
                    Calculate(defs(commandSections(1)), graphic, defs, New Rectangle(GetPoint(commandSections(2), region.Width) + region.X, GetPoint(commandSections(3), region.Height) + region.Y, GetPoint(commandSections(4), region.Width), GetPoint(commandSections(5), region.Height)))
                End If
            Case "drawarc"
                If commandSections.Length <> 9 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(5)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                For Each i As Char In commandSections(6)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                For Each i As Char In commandSections(7)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.DrawArc(New Pen(GetColor(commandSections(8)), UInt64.Parse(commandSections(7))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height), Integer.Parse(commandSections(5)), Integer.Parse(commandSections(6)))
            Case "drawbezier"
                If commandSections.Length <> 11 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(9)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.DrawBezier(New Pen(GetColor(commandSections(10)), UInt64.Parse(commandSections(9))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width) + region.X, GetPoint(commandSections(4), region.Height) + region.Y, GetPoint(commandSections(5), region.Width) + region.X, GetPoint(commandSections(6), region.Height) + region.Y, GetPoint(commandSections(7), region.Width) + region.X, GetPoint(commandSections(8), region.Height) + region.Y)
            Case "drawbeziers"
                If commandSections.Length Mod 2 = 0 Or commandSections.Length <= 5 Then Return
                If ((commandSections.Length - 3) / 2) Mod 3 <> 1 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(commandSections.Length - 2)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                Dim pointCoordinates(commandSections.Length - 4) As String
                For i As UInt64 = 0 To commandSections.Length - 4
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.DrawBeziers(New Pen(GetColor(commandSections(commandSections.Length - 1)), UInt64.Parse(commandSections(commandSections.Length - 2))), GetPoints(pointCoordinates, region))
            Case "drawclosedcurve"
                If commandSections.Length Mod 2 = 0 Or commandSections.Length <= 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(commandSections.Length - 2)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                Dim pointCoordinates(commandSections.Length - 4) As String
                For i As UInt64 = 0 To commandSections.Length - 4
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.DrawClosedCurve(New Pen(GetColor(commandSections(commandSections.Length - 1)), UInt64.Parse(commandSections(commandSections.Length - 2))), GetPoints(pointCoordinates, region))
            Case "fillclosedcurve"
                If commandSections.Length Mod 2 = 1 Or commandSections.Length <= 6 Then Return
                Dim pointCoordinates(commandSections.Length - 3) As String
                For i As UInt64 = 0 To commandSections.Length - 3
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.FillClosedCurve(New SolidBrush(GetColor(commandSections(commandSections.Length - 1))), GetPoints(pointCoordinates, region))
            Case "drawcurve"
                If commandSections.Length Mod 2 = 0 Or commandSections.Length <= 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(commandSections.Length - 2)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                Dim pointCoordinates(commandSections.Length - 4) As String
                For i As UInt64 = 0 To commandSections.Length - 4
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.DrawCurve(New Pen(GetColor(commandSections(commandSections.Length - 1)), UInt64.Parse(commandSections(commandSections.Length - 2))), GetPoints(pointCoordinates, region))
            Case "drawellipse"
                If commandSections.Length <> 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(5)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.DrawEllipse(New Pen(GetColor(commandSections(6)), UInt64.Parse(commandSections(5))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height))
            Case "fillellipse"
                If commandSections.Length <> 6 Then Return
                graphic.FillEllipse(New SolidBrush(GetColor(commandSections(commandSections.Length - 1))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height))
            Case "drawimage"
                If commandSections.Length < 2 Then Return
                If IO.File.Exists(commandSections(1)) Then
                    Select Case commandSections.Length
                        Case 4
                            graphic.DrawImage(Bitmap.FromFile(commandSections(1)), GetPoint(commandSections(2), region.Width) + region.X, GetPoint(commandSections(3), region.Height) + region.Y)
                        Case 6
                            graphic.DrawImage(Bitmap.FromFile(commandSections(1)), GetPoint(commandSections(2), region.Width) + region.X, GetPoint(commandSections(3), region.Height) + region.Y, GetPoint(commandSections(4), region.Width), GetPoint(commandSections(5), region.Height))
                        Case 8
                            graphic.DrawImage(Bitmap.FromFile(commandSections(1)), {New Point(GetPoint(commandSections(2), region.Width) + region.X, GetPoint(commandSections(3), region.Height) + region.Y), New Point(GetPoint(commandSections(4), region.Width) + region.X, GetPoint(commandSections(5), region.Height) + region.Y), New Point(GetPoint(commandSections(6), region.Width) + region.X, GetPoint(commandSections(7), region.Height) + region.Y)})
                        Case 12
                            Dim drawingBitmap As Bitmap = Bitmap.FromFile(commandSections(1))
                            graphic.DrawImage(drawingBitmap, {New Point(GetPoint(commandSections(2), region.Width) + region.X, GetPoint(commandSections(3), region.Height) + region.Y), New Point(GetPoint(commandSections(4), region.Width) + region.X, GetPoint(commandSections(5), region.Height) + region.Y), New Point(GetPoint(commandSections(6), region.Width) + region.X, GetPoint(commandSections(7), region.Height) + region.Y)}, New Rectangle(GetPoint(commandSections(8), drawingBitmap.Width - 1), GetPoint(commandSections(9), drawingBitmap.Height - 1), GetPoint(commandSections(10), drawingBitmap.Width - 1), GetPoint(commandSections(11), drawingBitmap.Height - 1)), GraphicsUnit.Pixel)
                    End Select
                End If
            Case "drawline"
                If commandSections.Length <> 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(5)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.DrawLine(New Pen(GetColor(commandSections(6)), UInt64.Parse(commandSections(5))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width) + region.X, GetPoint(commandSections(4), region.Height) + region.Y)
            Case "drawlines"
                If commandSections.Length Mod 2 = 0 Or commandSections.Length <= 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(commandSections.Length - 2)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                Dim pointCoordinates(commandSections.Length - 4) As String
                For i As UInt64 = 0 To commandSections.Length - 4
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.DrawLines(New Pen(GetColor(commandSections(commandSections.Length - 1)), UInt64.Parse(commandSections(commandSections.Length - 2))), GetPoints(pointCoordinates, region))
            Case "drawpie"
                If commandSections.Length <> 9 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(5)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                For Each i As Char In commandSections(6)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                For Each i As Char In commandSections(7)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.DrawPie(New Pen(GetColor(commandSections(8)), UInt64.Parse(commandSections(7))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height), Integer.Parse(commandSections(5)), Integer.Parse(commandSections(6)))
            Case "fillpie"
                If commandSections.Length <> 8 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(5)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                For Each i As Char In commandSections(6)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.FillPie(New SolidBrush(GetColor(commandSections(8))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height), Integer.Parse(commandSections(5)), Integer.Parse(commandSections(6)))
            Case "drawpolygon"
                If commandSections.Length Mod 2 = 0 Or commandSections.Length <= 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(commandSections.Length - 2)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                Dim pointCoordinates(commandSections.Length - 4) As String
                For i As UInt64 = 0 To commandSections.Length - 4
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.DrawPolygon(New Pen(GetColor(commandSections(commandSections.Length - 1)), UInt64.Parse(commandSections(commandSections.Length - 2))), GetPoints(pointCoordinates, region))
            Case "fillpolygon"
                If commandSections.Length Mod 2 = 1 Or commandSections.Length <= 6 Then Return
                Dim pointCoordinates(commandSections.Length - 3) As String
                For i As UInt64 = 0 To commandSections.Length - 3
                    pointCoordinates(i) = commandSections(i + 1)
                Next
                graphic.FillPolygon(New SolidBrush(GetColor(commandSections(commandSections.Length - 1))), GetPoints(pointCoordinates, region))
            Case "drawrectangle"
                If commandSections.Length <> 7 Then Return
                Dim bad As Boolean = False
                For Each i As Char In commandSections(5)
                    If Not numbers.Contains(i) Then
                        bad = True
                        Exit For
                    End If
                Next
                If bad Then Return
                graphic.DrawRectangle(New Pen(GetColor(commandSections(6)), UInt64.Parse(commandSections(5))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height))
            Case "fillrectangle"
                If commandSections.Length <> 6 Then Return
                graphic.FillRectangle(New SolidBrush(GetColor(commandSections(5))), GetPoint(commandSections(1), region.Width) + region.X, GetPoint(commandSections(2), region.Height) + region.Y, GetPoint(commandSections(3), region.Width), GetPoint(commandSections(4), region.Height))
            Case Else
                Return
        End Select
    End Sub
    Private Function GetPoints(parameters As String(), region As Rectangle) As Point()
        Dim count As UInt64 = Math.Floor(parameters.Length / 2)
        Dim points(count - 1) As Point
        For i As UInt64 = 0 To count - 1
            points(i) = New Point(GetPoint(parameters(i * 2), region.Width) + region.X, GetPoint(parameters((i * 2) + 1), region.Height) + region.Y)
        Next
        Return points
    End Function
    Private Function GetPoint(value As String, total As Int64) As Int64
        Dim fraction As Boolean = value.EndsWith("f"c)
        If fraction Then
            value = value.Substring(0, value.Length - 1)
            Dim seenDecimal As Boolean = False
            Dim notSeenNumber As Boolean = True
            For Each i As Char In value
                If i = "." And seenDecimal = False Then
                    seenDecimal = True
                ElseIf numbers.Contains(i) Then
                    notSeenNumber = False
                Else
                    Return 0
                End If
            Next
            If notSeenNumber Then Return 0
            Return (Double.Parse(value) * total) - 1
        Else
            For Each i As Char In value
                If Not numbers.Contains(i) Then Return 0
            Next
            Return Int64.Parse(value)
        End If
    End Function
    Private Function GetColor(colorString As String) As Color
        Try
            Return ColorTranslator.FromHtml(colorString)
        Catch ex As Exception
            Return Color.PaleVioletRed
        End Try
    End Function
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        If (Not IsNothing(Program.outputArgs)) AndAlso Program.outputArgs.Length > 0 AndAlso IO.File.Exists(Program.outputArgs(0)) Then
            fileLocation = Program.outputArgs(0)
            Dim text As String = IO.File.ReadAllText(fileLocation)
            Dim sections As String() = text.Split(","c)
            If sections.Length < 3 Then Return
            TextBox1.Text = sections(0)
            TextBox2.Text = sections(1)
            RichTextBox1.Text = text.Substring(sections(0).Length + sections(1).Length + 2)
        End If
    End Sub
    Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        Me.ActiveControl = Nothing
    End Sub
    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        MessageBox.Show(My.Resources.documentation, "Documentation")
    End Sub
End Class