(*
    SVG library
    ===========

    Author: lhl2617

    Note that all coordinates are in units. Pixels per unit is changed by
    altering the `unitPx` constant.
*)

module rec Verishot.SVG

type Coord = float * float 
type PropType = (string * string) list
type SVGElement = 
    // non-container element
    | Circle of 
        cxy: Coord * 
        r: float * 
        props: PropType *
        title: string Option
    | Rectangle of 
        xy: Coord * 
        wh: Coord * 
        props: PropType *
        title: string Option
    | Text of 
        xy: Coord * 
        text: string * 
        props: PropType *
        title: string Option
    | Polyline of 
        pts: Coord list * 
        props: PropType *
        title: string Option
    // container element (‘a’, ‘clipPath’, ‘defs’, ‘g’, ‘marker’, ‘mask’, ‘pattern’, ‘svg’, ‘switch’, ‘symbol’ and ‘unknown’.)
    | Group of 
        children: SVGElement list *
        props: PropType *
        title: string Option
    | Link of // a
        href: string *
        children: SVGElement list *
        props: PropType *
        title: string Option
      

let unitPx = 12. // pixels per drawing unit

let defaultCoord = 0., 0.

let groupSVG props title svgList =
    Group (svgList, props, title)

let linkSVG href props title svgList =
    Link (href, svgList, props, title)
    
let (|TOPX|) u = sprintf "%.1f" <| u * unitPx
    
let (|UPROPS|) (props: PropType) =
    props 
    |> List.map (fun (a, b) -> a + "='" + b + "'")
    |> String.concat " "
 
let (|UPOINTS|) (points: Coord list) =
    points
    |> List.map (fun (x, y) -> sprintf "%s,%s" ((|TOPX|) x) ((|TOPX|) y))
    |> String.concat " "

let (|PTITLE|) (title: string Option): string =
    match title with
    | Some str -> sprintf "<title>%s</title>" str
    | _ -> ""

let (|OUTPUTSVGLIST|) (elems: SVGElement list): string =
    elems
    |> List.map outputSVG
    |> String.concat "\n"  
    
let outputSVG (elem: SVGElement): string =
    match elem with
    | Circle ((TOPX cx, TOPX cy), TOPX r, UPROPS props, PTITLE title) -> 
        sprintf "<circle cx='%s' cy='%s' r='%s' %s>%s</circle>" 
            cx cy r props title
    | Rectangle ((TOPX x, TOPX y), (TOPX w, TOPX h), UPROPS props, PTITLE title) -> 
        sprintf "<rect x='%s' y='%s' width='%s' height='%s' %s>%s</rect>"
            x y w h props title
    | Text ((TOPX x, TOPX y), text, UPROPS props, PTITLE title) -> 
        sprintf "<text x='%s' y='%s' %s>%s%s</text>"
            x y props text title
    | Polyline (UPOINTS pts, UPROPS props, PTITLE title) ->
        sprintf "<polyline points='%s' %s>%s</polyline>"
            pts props title
    | Group (OUTPUTSVGLIST children, UPROPS props, PTITLE title) ->
        sprintf "<g %s>%s\n%s\n</g>"
            props title children
    | Link (href, OUTPUTSVGLIST children, UPROPS props, PTITLE title) ->
        sprintf "<a href='%s' %s>%s\n%s\n</a>"
            href props title children 

let getDimensionElem (elem: SVGElement): Coord * Coord = 
    match elem with
    | Circle ((cx, cy), r, _, _) -> 
        (cx-r, cy-r), (cx+r, cy+r)
    | Rectangle ((x, y), (w, h), _, _) -> 
        (x, y), (x+w, y+h)
    | Text ((x, y), _, _, _) ->
        (x, y), (x, y) // assume text always fits
    | Polyline (pts, _, _) -> 
        match List.isEmpty pts with
        | true -> (defaultCoord, defaultCoord)
        | _ -> 
            let minX = (List.map fst >> List.min) pts
            let minY = (List.map snd >> List.min) pts
            let maxX = (List.map fst >> List.max) pts
            let maxY = (List.map snd >> List.max) pts
            (minX, minY), (maxX, maxY)
    | Group (children, _, _)
    | Link (_, children, _, _) ->
        getDimensionElemList children
        

let getDimensionElemList (elems: SVGElement list): Coord * Coord = 
    match List.isEmpty elems with
    | true -> (defaultCoord, defaultCoord)
    | _ -> 
        let xyRange = List.map getDimensionElem elems
        let minRange = List.map fst xyRange
        let maxRange = List.map snd xyRange
        let minX = (List.map fst >> List.min) minRange
        let minY = (List.map snd >> List.min) minRange
        let maxX = (List.map fst >> List.max) maxRange
        let maxY = (List.map snd >> List.max) maxRange
        ((minX, minY), (maxX, maxY))

let translateCoord (xOffset, yOffset) (x, y) = 
    x + xOffset, y + yOffset

let translateSVGList offset (elems: SVGElement list) : SVGElement list =
    elems 
    |> List.map (translateSVG offset)

let translateSVG offset (elem: SVGElement): SVGElement =
    let (|TRANSCOORD|) = translateCoord offset
    let (|TRANSCOORDS|) pts = pts |> List.map (|TRANSCOORD|)
    let (|TRANSSVGLIST|) = translateSVGList offset
    match elem with
    | Circle (TRANSCOORD coord , a, b, c) -> 
        Circle (coord, a, b, c)
    | Rectangle (TRANSCOORD coord, a, b, c) ->
        Rectangle (coord, a, b, c)
    | Text (TRANSCOORD coord, a, b, c) ->
        Text (coord, a, b, c)
    | Polyline (TRANSCOORDS pts, a, b) ->
        Polyline (pts, a, b)
    | Group (TRANSSVGLIST children, a, b) ->
        Group (children, a, b)
    | Link (a, TRANSSVGLIST children, b, c) -> 
        Link (a, children, b, c)
    
let getGrid ((minX, minY), (maxX, maxY)): SVGElement =
    let xs = [minX .. 1. .. maxX]
    let ys = [minY .. 1. .. maxY]

    let getGridDot (x, y) =
        Circle((x, y), 0.125, [("style", "fill: #E0E0E0;")], None)

    let gridDots = List.allPairs xs ys |> List.map getGridDot 

    Group (gridDots, [], None) 

let output (elem: SVGElement) (style: string) (grid: bool): string =
    let (minX, minY), (maxX, maxY) = getDimensionElem elem

    let margin = 4.
    let w, h = maxX-minX+2.*margin, maxY-minY+2.*margin

    let viewBox = 
        [minX-margin; minY-margin; maxX+2.*margin; maxY+2.*margin]

    let gridOutput = 
        match grid with 
        | true -> 
            getGrid ((minX-margin/4., minY-margin/4.), (maxX+margin/4., maxY+margin/4.)) 
                |> outputSVG
        | _ ->
            ""
       
    let borderOutput = 
        Rectangle((minX-margin/2., minY-margin/2.), (maxX-minX+margin, maxY-minY+margin), [("style", "fill: none; stroke: black;")], None)
        |> outputSVG

    let svgOutput = outputSVG elem

    let viewBoxStr = viewBox |> List.map (|TOPX|) |> String.concat " "
    sprintf
        "<?xml version='1.0' encoding='UTF-8'?>\n"
        + "<!-- SVG Output - Verishot Simulator -->\n"
        + sprintf "<svg xmlns='http://www.w3.org/2000/svg' width='%s' height='%s' viewBox='%s'>\n" ((|TOPX|) w) ((|TOPX|) h) viewBoxStr
        + "<style type='text/css'>\n"
        + sprintf "%s\n" style
        + "</style>\n"
        + sprintf "%s\n" gridOutput
        + sprintf "%s\n" borderOutput
        + sprintf "%s\n" svgOutput
        + "</svg>"