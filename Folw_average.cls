VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "ThisWorkbook"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
Sub Flow()

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
Mon = Month(Cells(2, 1))
k = 1

Range("A1").Select

a = 2
b = 1
c = 2
d = 4

For k = 1 To 1000

    Do Until Month(Application.Worksheets("total").Cells(a, 1)) <> Mon
        Total = Total + Cells(a, 2)
        Count = Count + 1
        a = a + 1
    Loop
    
    
       
    ActiveCell.Cells(c, 5) = (Total / Count) / 28.3168  'Convert cubic feet to liter
    ActiveCell.Cells(c, 4) = Month(Cells(a - 1, 1)) & "-" & Year(Cells(a - 1, 1))
    c = c + 1
    
    Total = 0
    Count = 0
    
    Mon = Month(Cells(a, 1))
    
    If Mon > 12 Then
    Mon = 1
    End If
    
    
Next k


End Sub
