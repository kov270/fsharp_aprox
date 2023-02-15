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
        let f = linear points
        Assert.Equal(-1., f -1.)
        Assert.Equal(10.3, f 10.3)

    [<Fact>]
    let ``test segment`` () =
        let points = [ (3., 4.); (2., 2.); (1., 2.); (0., -1.) ]
        let f = segment points
        Assert.Equal(-1., f -1.)
        Assert.Equal(4., f 10.3)
        Assert.Equal(3., f 2.5)
        Assert.Equal(2., f 1.5)
