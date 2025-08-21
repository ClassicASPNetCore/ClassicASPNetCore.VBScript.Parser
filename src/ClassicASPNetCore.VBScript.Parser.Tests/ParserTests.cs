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
    [InlineData("1 + 2")]
    [InlineData("10 * 5")]
    [InlineData("a > b")]
    public void ParseString_ShouldParseSimpleBinaryExpressions(string expressionCode)
    {
        // ARRANGE
        var code = $"x = {expressionCode}";

        // ACT
        var ast = VBScriptParser.parseString(code);

        // ASSERT
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);

        Assert.IsType<Expr.BinaryOperation>(letStatement.value);
    }

    [Fact]
    public void ParseString_ShouldParseDateLiteral()
    {
        // ARRANGE
        var code = "miFecha = #12/31/2025#";

        // ACT
        var ast = VBScriptParser.parseString(code);

        // ASSERT
        var statement = Assert.Single(ast);
        var letStatement = Assert.IsType<Statement.Let>(((TopLevelItem.Statement)statement).Item);

        var literalExpr = Assert.IsType<Expr.Literal>(letStatement.value);
        
        var dateValue = Assert.IsType<Value.Date>(literalExpr.Item);
        
        Assert.Equal("12/31/2025", dateValue.Item);
    }
}
