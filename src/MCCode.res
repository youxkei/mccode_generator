open Belt.Array
let to_s = Js.Float.toString

@bs.val external pi: float = "Math.PI"

let version = [0, 0, 0, 0] // 0b0000
let maskPattern = [0, 0, 0, 1] // 0b0001
let versionMaskPattern = version->concat(maskPattern)

type config =
  | Config({
      density: int,
      layerWidth: float,
      frameWidth: float,
      strokeWidth: float,
      vertexWidthRatio: float,
      lineDistance: float,
      slideBridge: bool,
    })

let layerBitsLength = (density, layerIndex) =>
  // layerIndex = 1: 0
  // layerIndex > 1: (2 * layerIndex - 1) * density - 3
  (2 * layerIndex - 1) * density - 3

let layersBitsLength = (density, numLayers) =>
  // Σ_(i = 2)^(numLayers) (2 * i - 1) * density - 3 = density * numLayers ^ 2 - 3 * numLayers + 3 - density
  density * numLayers * numLayers - 3 * numLayers + 3 - density

let requiredLayers = (density, numBits) => {
  let density = density->float_of_int
  let numBits = numBits->float_of_int

  // the positive root of the equation `density * x ^ 2 - 3 * x + 3 - density = numBits`
  ceil(
    (sqrt(4.0 *. density ** 2.0 +. 4.0 *. (numBits -. 3.0) *. density +. 9.0) +. 3.0) /.
      (2.0 *.
      density),
  )->int_of_float
}

let createPadding = (bitsLength, density, numLayers) => {
  let paddedLength = layersBitsLength(density, numLayers)

  make(paddedLength - bitsLength, 0)
}

let mask = (i, bit) =>
  if mod(i, 2) == 0 {
    bit
  } else {
    1 - bit
  }

let applyMaskPattern = bits => bits->mapWithIndex(mask)

let makeVertices = (n, Config({density, layerWidth, frameWidth, vertexWidthRatio})) => {
  let verticesNum = density * n

  let radius = layerWidth *. float_of_int(n)
  let radianDelta = frameWidth /. radius *. vertexWidthRatio /. 2.0

  let sx = radius *. cos(-.radianDelta)
  let sy = radius *. sin(-.radianDelta)
  let ex = radius *. cos(radianDelta)
  let ey = radius *. sin(radianDelta)

  range(0, verticesNum - 1)->map(i => {
    let degree = 360.0 /. float_of_int(verticesNum) *. float_of_int(i)

    <path
      d=`M ${sx->to_s},${sy->to_s} A ${radius->to_s},${radius->to_s} 0,0,1 ${ex->to_s},${ey->to_s}`
      transform=`rotate(${degree->to_s})`
      strokeWidth=`${frameWidth->to_s}`
      fill="none"
    />
  })->React.array
}

let makeEdges = (
  n,
  bits,
  Config({density, layerWidth, strokeWidth, lineDistance, slideBridge}),
) => {
  let strokeWidthString = strokeWidth->to_s

  let verticesNum = density * n
  let radian = 2.0 *. pi /. float_of_int(verticesNum)
  let radius = layerWidth *. float_of_int(n)

  let prevVetricesNum = max(density * (n - 1), 1)
  let prevRadian = 2.0 *. pi /. float_of_int(prevVetricesNum)
  let prevRadius = layerWidth *. float_of_int(n - 1)

  let yDelta = lineDistance /. 2.0

  let edge = (bx, by, ex, ey, index) =>
    if bits[index] == 1 {
      <line x1=bx y1=by x2=ex y2=ey strokeWidth=strokeWidthString fill="none" />
    } else {
      React.array([])
    }

  let edges = range(0, prevVetricesNum - 1)->map(i => {
    let indexAdjust = int_of_float(floor(float_of_int(i - 1) /. float_of_int(max(n - 1, 1))))
    let startIndex = i + indexAdjust

    let bx = (prevRadius *. cos(prevRadian *. float_of_int(i)))->to_s
    let by =  (prevRadius *. sin(prevRadian *. float_of_int(i)))->to_s
    let ex0 = (radius *. cos(radian *. float_of_int(startIndex)))->to_s
    let ey0 = (radius *. sin(radian *. float_of_int(startIndex)))->to_s
    let ex1 = (radius *. cos(radian *. float_of_int(startIndex + 1)))->to_s
    let ey1 = (radius *. sin(radian *. float_of_int(startIndex + 1)))->to_s
    let ex2 = (radius *. cos(radian *. float_of_int(startIndex + 2)))->to_s
    let ey2 = (radius *. sin(radian *. float_of_int(startIndex + 2)))->to_s

    if i == 0 {
      <>
        <line
          x1=bx
          y1=by
          x2=ex1
          y2=ey1
          transform=`translate(0, -${yDelta->to_s})`
          strokeWidth=strokeWidthString
          fill="none"
        />
        <line
          x1=bx
          y1=by
          x2=ex1
          y2=ey1
          transform=`translate(0, ${yDelta->to_s})`
          strokeWidth=strokeWidthString
          fill="none"
        />
        <line x1=bx y1=by x2=ex2 y2=ey2 strokeWidth=strokeWidthString fill="none" />
      </>
    } else {
      let firstBitsIndex = 2 * (i - 1) + indexAdjust

      if mod(i, n - 1) == 0 {
        <>
          {edge(bx, by, ex0, ey0, firstBitsIndex)}
          {edge(bx, by, ex1, ey1, firstBitsIndex + 1)}
          {edge(bx, by, ex2, ey2, firstBitsIndex + 2)}
        </>
      } else {
        <> {edge(bx, by, ex0, ey0, firstBitsIndex)} {edge(bx, by, ex1, ey1, firstBitsIndex + 1)} </>
      }
    }
  })->React.array

  let wholeDegree = if slideBridge {
    -360.0 /. float_of_int(density) *. float_of_int(n - 1)
  } else {
    0.0
  }
  <g transform=`rotate(${wholeDegree->to_s})`> edges </g>
}

let makeLayer = (
  n,
  bits,
  Config({layerWidth, frameWidth, strokeWidth, vertexWidthRatio}) as config,
) => {
  let strokeWidthString = strokeWidth->to_s

  let radius = layerWidth *. float_of_int(n)

  if n == 0 {
    <circle x="0" y="0" r={(frameWidth /. 2.0 *. vertexWidthRatio)->to_s} stroke="none" />
  } else {
    <>
      <circle
        cx="0"
        cy="0"
        r={(radius +. frameWidth /. 2.0)->to_s}
        strokeWidth=strokeWidthString
        fill="none"
      />
      <circle
        cx="0"
        cy="0"
        r={(radius -. frameWidth /. 2.0)->to_s}
        strokeWidth=strokeWidthString
        fill="none"
      />
      {makeVertices(n, config)}
      {makeEdges(n, bits, config)}
    </>
  }
}

@react.component
let make = (~config, ~bits) => {
  let Config({density, layerWidth, frameWidth}) = config

  // add version bits (4 bits) and mask pattern bits (4 bits)
  let bitsLength = bits->length + 8

  let numLayers = requiredLayers(density, bitsLength)
  let padding = createPadding(bitsLength, density, numLayers)

  let bits = versionMaskPattern->concat(bits->concat(padding)->applyMaskPattern)

  let radius = layerWidth *. numLayers->float_of_int +. frameWidth

  let width = (radius *. 2.0)->to_s

  <svg
    xmlns="http://www.w3.org/2000/svg"
    id="mccode"
    width="100%"
    height="100%"
    viewBox=`-${radius->to_s} -${radius->to_s} ${width} ${width}`
    stroke="black"
    fill="black">
    <rect x=`-${radius->to_s}` y=`-${radius->to_s}` width=width height=width stroke="none" fill="white" />
    {range(0, numLayers)->map(i => {
      let bits = if i >= 2 {
        let prevIndex = i - 1
        let firstBitsIndex = layersBitsLength(density, prevIndex)
        let layerNumBits = layerBitsLength(density, i)

        bits->slice(~offset=firstBitsIndex, ~len=layerNumBits)
      } else {
        []
      }

      makeLayer(i, bits, config)
    })->React.array}
  </svg>
}
