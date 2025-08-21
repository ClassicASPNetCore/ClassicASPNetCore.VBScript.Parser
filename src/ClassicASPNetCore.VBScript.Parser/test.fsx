#r "nuget: FParsec"
#load "Types.fs"
#load "ParserCore.fs"
#load "Expressions.fs"
#load "Statements.fs"
#load "Library.fs"

open ClassicASPNetCore.VBScript.Parser

let vbscriptCode = """
' Declaración de variables al inicio
Dim contador, suma, mensaje, estado, i
Dim miColeccion, elemento, valorTest

' Asignación de valores (Let) y de referencias (Set)
suma = 0
mensaje = "Iniciando proceso"
Set miColeccion = CreateObject ' Simulado

For i = 1 To 10 Step 2
    suma = suma + i * (2 + 1)
Next

If (suma > 20 And Not estado = True) Or mensaje = "Iniciando proceso" Then
    contador = 1
Else
    contador = 0
End If

While contador < 5
    contador = contador + 1
Wend

Do
    estado = True
Loop Until estado = True

Do While contador > 0
    contador = contador - 1
Loop

For Each elemento In miColeccion
    mensaje = "Item encontrado"
Next

valorTest = 5
Select Case valorTest
    Case 1, 2, 3
        mensaje = "Valor bajo"
    Case 4, 5
        mensaje = "Valor medio"
    Case Else
        mensaje = "Valor alto"
End Select
"""

let ast = VBScriptParser.parseString vbscriptCode

printfn "%A" ast