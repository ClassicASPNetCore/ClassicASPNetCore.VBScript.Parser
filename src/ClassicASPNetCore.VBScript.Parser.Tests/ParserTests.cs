namespace ClassicASPNetCore.VBScript.Parser.Tests;

using System.Linq;
using ClassicASPNetCore.VBScript.Parser;

public class ParserTests
{
    [Fact]
    public void ParseString_ShouldParseSingleDimStatement()
    {
        // ARRANGE
        var code = "Dim myVariable";

        // ACT
        var ast = VBScriptParser.parseString(code);

        // ASSERT
        var statement = Assert.Single(ast);

        var dimStatement = Assert.IsType<Statement.Dim>(((TopLevelItem.Statement)statement).Item);

        var variableName = Assert.Single(dimStatement.variables);
        Assert.Equal("myVariable", variableName);
    }

    [Fact]
    public void ParseString_ShouldParseSimpleLetAssignment()
    {
        // ARRANGE
        var code = "x = 123";

        // ACT
        var ast = VBScriptParser.parseString(code);

        // ASSERT
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);

        Assert.Equal("x", letStatement.name);

        var literalExpr = Assert.IsType<Expr.Literal>(letStatement.value);
        var integerValue = Assert.IsType<Value.Integer>(literalExpr.Item);
        Assert.Equal(123, integerValue.Item);
    }

    [Fact]
    public void ParseString_ShouldParseSimpleIfStatement()
    {
        // ARRANGE
        var code = @"
            If True Then
                Dim y
            End If
        ";

        // ACT
        var ast = VBScriptParser.parseString(code);

        // ASSERT
        var statement = Assert.Single(ast);
        var ifStatement = Assert.IsType<Statement.If>(((TopLevelItem.Statement)statement).Item);

        Assert.NotNull(ifStatement.body);

        var innerStatement = Assert.Single(ifStatement.body);

        Assert.IsType<Statement.Dim>(innerStatement);
    }

    [Theory]
    [InlineData("x = 123", typeof(Value.Integer), 123)]
    [InlineData("x = \"hello\"", typeof(Value.String), "hello")]
    [InlineData("x = True", typeof(Value.Boolean), true)]
    [InlineData("x = False", typeof(Value.Boolean), false)]
    [InlineData("x = #12/31/2025#", typeof(Value.Date), "12/31/2025")]
    public void ParseString_ShouldParseLiterals(string code, Type expectedValueType, object expectedValue)
    {
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);
        var literalExpr = Assert.IsType<Expr.Literal>(letStatement.value);
        
        Assert.IsType(expectedValueType, literalExpr.Item);
        var actualValue = literalExpr.Item.GetType().GetProperty("Item").GetValue(literalExpr.Item);
        Assert.Equal(expectedValue, actualValue);
    }

    private Operator GetOperator(string name) => (Operator)typeof(Operator).GetProperty(name).GetValue(null);

    [Theory]
    [InlineData("-x", "UnaryNegation")]
    [InlineData("Not True", "LogicalNegation")]
    public void ParseString_ShouldParseUnaryExpressions(string expressionCode, string expectedOpName)
    {
        var code = $"x = {expressionCode}";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);
        var unaryExpr = Assert.IsType<Expr.UnaryOperation>(letStatement.value);
        Assert.Equal(GetOperator(expectedOpName), unaryExpr.Item.Item1);
    }

    [Theory]
    [InlineData("1 + 2", "Addition")]
    [InlineData("1 - 2", "Subtraction")]
    [InlineData("1 * 2", "Multiplication")]
    [InlineData("1 / 2", "Division")]
    [InlineData("1 \\ 2", "IntegerDivision")]
    [InlineData("1 ^ 2", "Exponentiation")]
    [InlineData("1 Mod 2", "Modulus")]
    [InlineData("a & b", "StringConcatenation")]
    [InlineData("a = b", "Equality")]
    [InlineData("a <> b", "Inequality")]
    [InlineData("a <= b", "LessOrEqualThan")]
    [InlineData("a >= b", "GreaterOrEqualThan")]
    [InlineData("a < b", "LessThan")]
    [InlineData("a > b", "GreaterThan")]
    [InlineData("a Is b", "ObjectEquivalence")]
    [InlineData("a And b", "LogicalConjunction")]
    [InlineData("a Or b", "LogicalDisjunction")]
    [InlineData("a Xor b", "LogicalExclusion")]
    [InlineData("a Eqv b", "LogicalEquivalence")]
    [InlineData("a Imp b", "LogicalImplication")]
    public void ParseString_ShouldParseAllBinaryExpressions(string expressionCode, string expectedOpName)
    {
        var code = $"x = {expressionCode}";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);
        var binaryExpr = Assert.IsType<Expr.BinaryOperation>(letStatement.value);
        Assert.Equal(GetOperator(expectedOpName), binaryExpr.Item.Item2);
    }

    [Fact]
    public void ParseString_ShouldParseComplexExpressionWithParentheses()
    {
        var code = "x = (1 + 2) * 3";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);
        var binaryExpr = Assert.IsType<Expr.BinaryOperation>(letStatement.value);
        Assert.Equal(Operator.Multiplication, binaryExpr.Item.Item2);
        var left = Assert.IsType<Expr.BinaryOperation>(binaryExpr.Item.Item1);
        Assert.Equal(Operator.Addition, left.Item.Item2);
    }

    [Fact]
    public void ParseString_ShouldParseFunctionCall()
    {
        var code = "x = MyFunc(1, \"test\")";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);
        var callExpr = Assert.IsType<Expr.Call>(letStatement.value);
        Assert.Equal("MyFunc", callExpr.name);
        Assert.Equal(2, callExpr.args.Count());
    }

    [Fact]
    public void ParseString_ShouldParseSetStatement()
    {
        var code = "Set obj = Nothing";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var setStatement = Assert.IsType<Statement.Set>(((TopLevelItem.Statement)statement).Item);
        Assert.Equal("obj", setStatement.name);
    }

    [Fact]
    public void ParseString_ShouldParseComplexIfStatement()
    {
        var code = @"
            If x = 1 Then
                y = 1
            ElseIf x = 2 Then
                y = 2
            Else
                y = 3
            End If
        ";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var ifStatement = Assert.IsType<Statement.If>(((TopLevelItem.Statement)statement).Item);
        Assert.Single(ifStatement.elseIfs);
        Assert.NotNull(ifStatement.Else);
    }

    [Fact]
    public void ParseString_ShouldParseWhileLoop()
    {
        var code = @"
            While True
                x = x + 1
            Wend
        ";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var whileStatement = Assert.IsType<Statement.While>(((TopLevelItem.Statement)statement).Item);
        Assert.IsType<Expr.Literal>(whileStatement.condition);
    }

    [Fact]
    public void ParseString_ShouldParseForLoop()
    {
        var code = @"
            For i = 1 To 10 Step 2
                x = x + i
            Next
        ";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var forStatement = Assert.IsType<Statement.For>(((TopLevelItem.Statement)statement).Item);
        Assert.Equal("i", forStatement.counter);
        Assert.NotNull(forStatement.step);
    }

    [Fact]
    public void ParseString_ShouldParseForEachLoop()
    {
        var code = @"
            For Each item In collection
                x = x + 1
            Next
        ";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var forEachStatement = Assert.IsType<Statement.ForEach>(((TopLevelItem.Statement)statement).Item);
        Assert.Equal("item", forEachStatement.item);
    }

    [Theory]
    [InlineData("Do While True\nLoop", true, false)]
    [InlineData("Do Until True\nLoop", true, false)]
    [InlineData("Do\nLoop While True", false, true)]
    [InlineData("Do\nLoop Until True", false, true)]
    public void ParseString_ShouldParseDoLoop(string code, bool hasPreCond, bool hasPostCond)
    {
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var doLoopStatement = Assert.IsType<Statement.DoLoop>(((TopLevelItem.Statement)statement).Item);
        Assert.Equal(hasPreCond, doLoopStatement.preCondition != null);
        Assert.Equal(hasPostCond, doLoopStatement.postCondition != null);
    }

    [Fact]
    public void ParseString_ShouldParseSelectCase()
    {
        var code = @"
            Select Case x
                Case 1, 2
                    y = 1
                Case Else
                    y = 2
            End Select
        ";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var selectStatement = Assert.IsType<Statement.SelectCase>(((TopLevelItem.Statement)statement).Item);
        Assert.Single(selectStatement.cases);
        Assert.NotNull(selectStatement.defaultCase);
    }

    [Fact]
    public void ParseString_ShouldParseSubProcedure()
    {
        var code = @"
            Sub MySub(ByVal a, ByRef b)
                x = a + b
            End Sub
        ";
        var ast = VBScriptParser.parseString(code);
        var item = Assert.Single(ast);
        var procedure = Assert.IsType<TopLevelItem.Procedure>(item);
        var subRec = Assert.IsType<Procedure.Sub>(procedure.Item);
        Assert.Equal("MySub", subRec.name);
        Assert.Equal(2, subRec.args.Length);
        Assert.False(subRec.args[0].ByRef);
        Assert.True(subRec.args[1].ByRef);
    }

    [Fact]
    public void ParseString_ShouldParseFunctionProcedure()
    {
        var code = @"
            Function MyFunc()
                MyFunc = 1
            End Function
        ";
        var ast = VBScriptParser.parseString(code);
        var item = Assert.Single(ast);
        var procedure = Assert.IsType<TopLevelItem.Procedure>(item);
        var funcRec = Assert.IsType<Procedure.Function>(procedure.Item);
        Assert.Equal("MyFunc", funcRec.name);
        Assert.Empty(funcRec.args);
    }

    [Fact]
    public void ParseString_ShouldParseMultipleTopLevelItems()
    {
        var code = @"
            Dim x
            x = 1
            Sub MySub()
            End Sub
        ";
        var ast = VBScriptParser.parseString(code);
        Assert.Equal(3, ast.Count());
    }

    [Fact]
    public void ParseString_ShouldParseMultiStatementLines()
    {
        var code = "x = 1 : y = 2";
        var ast = VBScriptParser.parseString(code);
        Assert.Equal(2, ast.Count());
    }

    [Fact]
    public void ParseString_ShouldHandleLineContinuations()
    {
        var code = "x = 1 + _\n 2";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);
        var binaryExpr = Assert.IsType<Expr.BinaryOperation>(letStatement.value);
        Assert.Equal(Operator.Addition, binaryExpr.Item.Item2);
    }

    [Fact]
    public void ParseString_ShouldHandleComments()
    {
        var code = @"
            ' This is a comment
            x = 1 ' Another comment
            ' Final comment
        ";
        var ast = VBScriptParser.parseString(code);
        Assert.Single(ast);
    }

    [Fact]
    public void ParseString_ShouldParseExitStatement()
    {
        var code = "Exit Sub";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var exitStatement = Assert.IsType<Statement.Exit>(((TopLevelItem.Statement)statement).Item);
        Assert.Equal("Sub", exitStatement.Item);
    }

    [Fact]
    public void ParseString_ShouldParseCallStatement()
    {
        var code = "Call MySub(1)";
        var ast = VBScriptParser.parseString(code);
        var statement = Assert.Single(ast);
        var callStatement = Assert.IsType<Statement.CallStmt>(((TopLevelItem.Statement)statement).Item);
        var callExpr = Assert.IsType<Expr.Call>(callStatement.callExpr);
        Assert.Equal("MySub", callExpr.name);
    }
}
