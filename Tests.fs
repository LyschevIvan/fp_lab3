module Tests

open System
open System.IO
open Xunit
open Lab3
open Xunit.Abstractions

type Lab3Tests(output: ITestOutputHelper) =

    [<Fact>]
    let ``test linear`` () =
        let points = [ (3., 3.); (2., 2.); (1., 1.); (0., 0.) ]
        let f = getFunc 0 points
        Assert.Equal(-1., f -1.)
        Assert.Equal(10.3, f 10.3)

    [<Fact>]
    let ``test segment`` () =
        let points = [ (3., 4.); (2., 2.); (1., 2.); (0., -1.) ]
        let f = getFunc 1 points
        Assert.Equal(-1., f -1.)
        Assert.Equal(4., f 10.3)
        Assert.Equal(3., f 2.5)
        Assert.Equal(2., f 1.5)

    [<Fact>]
    let ``test factorial`` () =
        let points =
            [ (11., 14.591581091193483)
              (5., 11.437751649736402)
              (4., 10.545177444479563)
              (2., 7.772588722239782)
              (1., 5.) ]

        let f = getFunc 1 points
        let exp1: double = 14.591581091193483
        Assert.Equal(exp1, f 11, 7)
        let exp2: double = 11.437751649736402
        Assert.Equal(exp2, f 5, 7)
        let exp3: double = 5.
        Assert.Equal(exp3, f 1, 7)

    [<Theory>]
    [<InlineData("/log.txt", "0 2 1")>]
    [<InlineData("/linear.txt", "0")>]
    let ``test file read`` path (funcStr: string) =
        Console.SetIn(File.OpenText(__SOURCE_DIRECTORY__ + @"/testData/" + path))
        let writer = new StringWriter()
        Console.SetOut writer
        let funcIds = funcStr.Split(" ") |> Array.map int
        processFuncs funcIds 1 10 10
        output.WriteLine(writer.ToString())
