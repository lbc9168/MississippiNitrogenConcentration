Attribute VB_Name = "RibbonX_Code"
Sub N_Price()

Dim Total As Single
Dim Count As Single
Dim Mon As Integer
Dim a As Integer
Dim k As Integer
Dim b As Integer
Dim c As Integer
Dim d As Integer

Total = 0
Count = 0
Mon = 4
k = 1

Range("A1").Select

a = 2
b = 1
c = 2
d = 4

For k = 1 To 887

    Do Until Month(Application.Worksheets("N_Price").Cells(a, 1)) <> Mon
        Total = Total + Cells(a, 2)
        Count = Count + 1
        a = a + 1
    Loop
    
    
       
    ActiveCell.Cells(c, 7) = Total / Count
    ActiveCell.Cells(c, 6) = Month(Cells(a - 1, 1)) & "-" & Year(Cells(a - 1, 1))
    c = c + 1
    
    Total = 0
    Count = 0
    
    Mon = Month(Cells(a, 1))
    
    If Mon > 9 Then
    Mon = 4
    End If
    
    
Next k



End Sub

