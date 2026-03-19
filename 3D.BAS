CONST SCREEN.WIDTH = 320
CONST SCREEN.HEIGHT = 200
CONST PI = 3.14

TYPE Vertex
  x AS SINGLE
  y AS SINGLE
  z AS SINGLE
END TYPE

TYPE Vector
  x AS SINGLE
  y AS SINGLE
  z AS SINGLE
END TYPE

TYPE Triangle
  v1 AS INTEGER
  v2 AS INTEGER
  v3 AS INTEGER
  normal AS Vector
  avgZ AS SINGLE
END TYPE

'Function declerations
DECLARE SUB ApplyPerspective (verts() AS Vertex, size AS INTEGER)
DECLARE SUB DrawModel (model() AS Triangle, modelSize AS INTEGER, verts() AS Vertex)
DECLARE SUB CopyVerts (src() AS Vertex, dest() AS Vertex)
DECLARE SUB LoadModel (verts() AS Vertex, vertsSize AS INTEGER, model() AS Triangle, modelSize AS INTEGER)
DECLARE SUB Rotate (angle AS SINGLE, verts() AS Vertex, size AS INTEGER)
DECLARE SUB TransformWorld (verts() AS Vertex, size AS INTEGER)
DECLARE SUB CalcNormals (model() AS Triangle, verts() AS Vertex, modelSize AS INTEGER)
DECLARE SUB CullFaces (model() AS Triangle, modelSize AS INTEGER, xModel() AS Triangle, xModelSize AS INTEGER, xVerts() AS ANY)
DECLARE SUB FillTriangle (argV1 AS Vertex, argV2 AS Vertex, argV3 AS Vertex, colour AS INTEGER)
DECLARE SUB DrawTriangle (srcTri AS Triangle, verts() AS Vertex, colour AS INTEGER)
DECLARE SUB FillTriangle2 (argV1 AS Vertex, argV2 AS Vertex, argV3 AS Vertex, colour AS INTEGER)
DECLARE SUB InsertTriangle (tri AS Triangle, model() AS Triangle, length AS INTEGER)

DIM verts(1 TO 200) AS Vertex
DIM xVerts(1 TO 200) AS Vertex
DIM xVertsSize  AS INTEGER

DIM model(500) AS Triangle
DIM modelSize AS INTEGER
DIM xModel(500) AS Triangle
DIM xModelSize AS INTEGER

CALL LoadModel(verts(), xVertsSize, model(), modelSize)

SCREEN 7, 0, 0, 1

DIM startTime AS SINGLE, endTime AS SINGLE
DIM angle AS SINGLE

DO
  startTime = TIMER
  CLS
  CALL CopyVerts(verts(), xVerts())
  CALL Rotate(angle, xVerts(), xVertsSize)
  CALL TransformWorld(xVerts(), xVertsSize)
  CALL CalcNormals(model(), xVerts(), modelSize)
  CALL CullFaces(model(), modelSize, xModel(), xModelSize, xVerts())
  CALL ApplyPerspective(xVerts(), xVertsSize)
  CALL DrawModel(xModel(), xModelSize, xVerts())
  
  endTime = TIMER
  PRINT "Frame time: "; endTime - startTime
  PRINT "Faces culled: "; modelSize - xModelSize
 
  PCOPY 0, 1

  angle = angle + (PI / 32)
LOOP UNTIL INKEY$ <> ""

SUB ApplyPerspective (verts() AS Vertex, size AS INTEGER)
  CONST hw = SCREEN.WIDTH / 2
  DIM V AS Vertex

  FOR i = LBOUND(verts) TO size + 1
    V = verts(i)
    verts(i).x = hw * V.x / V.z
    verts(i).y = hw * V.y / V.z
  NEXT i
END SUB

SUB CalcNormals (model() AS Triangle, verts() AS Vertex, modelSize AS INTEGER)
  DIM f AS Triangle
  DIM v1 AS Vertex, v2 AS Vertex, v3 AS Vertex
  DIM ax AS SINGLE, ay AS SINGLE, az AS SINGLE
  DIM bx AS SINGLE, by AS SINGLE, bz AS SINGLE
  DIM nx AS SINGLE, ny AS SINGLE, nz AS SINGLE

  FOR j = 0 TO modelSize - 1
    f = model(j)
    v1 = verts(f.v1)
    v2 = verts(f.v2)
    v3 = verts(f.v3)

    'Edge vector A V3 - V1
    ax = v3.x - v1.x
    ay = v3.y - v1.y
    az = v3.z - v1.z

    'Edge vector B V2 - V1
    bx = v2.x - v1.x
    by = v2.y - v1.y
    bz = v2.z - v1.z

    'Calc cross product
    nx = (ay * bz) - (az * by)
    ny = (az * bx) - (ax * bz)
    nz = (ax * by) - (ay * bx)

    'Normalize vector
    length = SQR(nx * nx + ny * ny + nz * nz)

    IF length > 0 THEN
      model(j).normal.x = nx / length
      model(j).normal.y = ny / length
      model(j).normal.z = nz / length
    END IF
  NEXT
 
END SUB

SUB CopyVerts (src() AS Vertex, dest() AS Vertex)
  FOR i = LBOUND(src) TO UBOUND(src)
    dest(i) = src(i)
  NEXT
END SUB

SUB CullFaces (model() AS Triangle, modelSize AS INTEGER, xModel() AS Triangle, xModelSize AS INTEGER, xVerts() AS Vertex)
  DIM j AS INTEGER
  DIM dotPrd AS SINGLE
  DIM vtx AS Vertex
  DIM camVtr AS Vector
  DIM normal AS Vector
  DIM x AS SINGLE, y AS SINGLE, z AS SINGLE
  DIM v1 AS Vertex, v2 AS Vertex, v3 AS Vertex

  FOR i = 0 TO modelSize - 1
    'The camera vector is the normalised vector from the origin to the first vertex of the triangle
    vtx = xVerts(model(i).v1)
    x = vtx.x
    y = vtx.y
    z = vtx.z

    length = SQR(x * x + y * y + z * z)
    camVtr.x = x / length
    camVtr.y = y / length
    camVtr.z = z / length

    normal = model(i).normal

    dotProd = (camVtr.x * normal.x) + (camVtr.y * normal.y) + (camVtr.z * normal.z)

    IF dotProd > 0 THEN
       v1 = xVerts(model(i).v1)
       v2 = xVerts(model(i).v2)
       v3 = xVerts(model(i).v3)
       model(i).avgZ = (v1.z + v2.z + v3.z) / 3
       CALL InsertTriangle(model(i), xModel(), j)
       j = j + 1
      
    END IF
  NEXT

  xModelSize = j

  'Use merge sort to sort model into sortedModel
  'CALL TopDownSplit(model(), sortedModel(), 0, modelSize + 1)

  'Use quick sort to sort the faces
  'CALL QuickSort(model(), 0, modelSize - 1)

  'DIM lastZ AS SINGLE
  'lastZ = -100000

  'FOR i = 0 TO modelSize
    'IF lastZ > sortedModel(i).avgZ THEN
     ' PRINT "ERROR!!!"
    'END IF

    'lastZ = sortedModel(i).avgZ
  'NEXT
END SUB

SUB DrawModel (model() AS Triangle, modelSize AS INTEGER, verts() AS Vertex)
  DIM n AS Vector, light AS Vector
  DIM colour AS INTEGER
 
  FOR i = 0 TO modelSize - 1
    n = model(i).normal
    'light.x = .3
    'light.y = -.3
    'light.z = .91
    light.x = .577
    light.y = -.577
    light.z = .577
    'light.x = .7
    'light.y = -.7
    'light.z = .14


    dotProduct = (n.x * light.x) + (n.y * light.y) + (n.z * light.z)

    IF dotProduct < .05 THEN
      colour = 8
    ELSEIF dotProduct < .2 THEN
      colour = 7
    ELSEIF dotProduct < .4 THEN
      colour = 3
    ELSEIF dotProduct < .55 THEN
      colour = 1
    ELSEIF dotProduct < .65 THEN
      colour = 9
    ELSEIF dotProduct < .87 THEN
      colour = 11
    ELSE
      colour = 15
    END IF
   
    CALL DrawTriangle(model(i), verts(), colour)
  NEXT i
END SUB

SUB DrawTriangle (srcTri AS Triangle, verts() AS Vertex, colour AS INTEGER)
  CONST hw = SCREEN.WIDTH / 2
  CONST hh = SCREEN.HEIGHT / 2

  DIM v1 AS Vertex, v2 AS Vertex, v3 AS Vertex, v4 AS Vertex
  DIM my AS INTEGER

  v1 = verts(srcTri.v1)
  v2 = verts(srcTri.v2)
  v3 = verts(srcTri.v3)

  ' Sort the verticies by y value
  IF v1.y < v2.y THEN
    SWAP v1, v2
  END IF

  IF v2.y < v3.y THEN
    SWAP v2, v3
  END IF

  IF v1.y < v2.y THEN
    SWAP v1, v2
  END IF

  ' Calclculate the x coordinate of the point of the edge V1 - V3
  v4.y = v2.y

  IF (v3.y - v1.y) <> 0 THEN
    v4.x = v1.x + ((v2.y - v1.y) / (v3.y - v1.y)) * (v3.x - v1.x)
  ELSE
    v4.x = v1.x 'Triangle is just a horizontal line
  END IF
 
  CALL FillTriangle(v1, v2, v4, colour)
  CALL FillTriangle2(v3, v2, v4, colour)
END SUB

'V1 should be the point of the triangle
SUB FillTriangle (argV1 AS Vertex, argV2 AS Vertex, argV3 AS Vertex, colour AS INTEGER)
  CONST hw = SCREEN.WIDTH / 2
  CONST hh = SCREEN.HEIGHT / 2

  DIM v1 AS Vertex, v2 AS Vertex, v3 AS Vertex
  v1 = argV1
  v2 = argV2
  v3 = argV3

  'Fix order of V2 and V3 by x value
  IF v2.x > v3.x THEN
    SWAP v2, v3
  END IF


  'Calculate the delta for each side
  DIM dLeft AS SINGLE, dRight AS SINGLE
  DIM height AS INTEGER, stp AS INTEGER

  height = v1.y - v3.y
  dLeft = (v2.x - v1.x) / height
  dRight = (v3.x - v1.x) / height

  DIM x1 AS SINGLE, x2 AS SINGLE
  x1 = v1.x
  x2 = v1.x

  IF height < 0 THEN
    stp = 1
  ELSE
    stp = -1
    'SWAP dLeft, dRight
  END IF

  FOR y = v1.y TO v1.y - height STEP stp
    LINE (x1 + hw, -1 * y + hh)-(x2 + hw, -1 * y + hh), colour
    x1 = x1 + dLeft
    x2 = x2 + dRight
  NEXT

END SUB

SUB FillTriangle2 (argV1 AS Vertex, argV2 AS Vertex, argV3 AS Vertex, colour AS INTEGER)
  CONST hw = SCREEN.WIDTH / 2
  CONST hh = SCREEN.HEIGHT / 2

  DIM v1 AS Vertex, v2 AS Vertex, v3 AS Vertex
  v1 = argV1
  v2 = argV2
  v3 = argV3

  'Fix order of V2 and V3 by x value
  IF v2.x > v3.x THEN
    SWAP v2, v3
  END IF


  'Calculate the delta for each side
  DIM dLeft AS SINGLE, dRight AS SINGLE
  DIM height AS INTEGER, stp AS INTEGER

  height = v3.y - v1.y
  dLeft = (v2.x - v1.x) / height
  dRight = (v3.x - v1.x) / height

  DIM x1 AS SINGLE, x2 AS SINGLE
  x1 = v1.x
  x2 = v1.x

  FOR y = v1.y TO v1.y + height
    LINE (x1 + hw, -1 * y + hh)-(x2 + hw, -1 * y + hh), colour
    x1 = x1 + dLeft
    x2 = x2 + dRight
  NEXT


END SUB

'Insert triangles in order
SUB InsertTriangle (tri AS Triangle, model() AS Triangle, length AS INTEGER)
  FOR i = 0 TO length - 1
    IF tri.avgZ > model(i).avgZ THEN 'Found spot to insert tri
      FOR j = length TO i + 1 STEP -1 'Shuffle existing items forward
       SWAP model(j), model(j - 1)
      NEXT
      model(i) = tri
      EXIT FOR
    END IF
  NEXT

  IF i = length THEN
    model(length) = tri
  END IF

  'FOR i = 1 TO length
  '  IF model(i).avgZ > model(i - 1).avgZ THEN
  '    PRINT "error!"
  '  END IF
  'NEXT
END SUB

SUB LoadModel (verts() AS Vertex, vertsSize AS INTEGER, model() AS Triangle, modelSize AS INTEGER)
  DIM rawLine AS STRING
  DIM prefix AS STRING
  DIM spPos AS INTEGER
  DIM vIdx AS INTEGER, fIdx AS INTEGER
  DIM val1 AS STRING, val2 AS STRING, val3 AS STRING

  'OPEN "\models\sphere.obj" FOR INPUT AS #1
  'OPEN "\models\cube.obj" FOR INPUT AS #1
  'OPEN "\models\simple.obj" FOR INPUT AS #1
  OPEN "\models\spitf3.obj" FOR INPUT AS #1

  vIdx = 1

  DO UNTIL EOF(1)
    LINE INPUT #1, rawLine

    prefix = LEFT$(rawLine, 1)
    rawLine = MID$(rawLine, 3)

    IF prefix = "v" OR prefix = "f" THEN
        spPos = INSTR(rawLine, " ")
        val1 = LEFT$(rawLine, spPos - 1)
        rawLine = MID$(rawLine, spPos + 1)
 
        spPos = INSTR(rawLine, " ")
        val2 = LEFT$(rawLine, spPos - 1)
        rawLine = MID$(rawLine, spPos + 1)

        val3 = rawLine
    END IF

    SELECT CASE prefix
      CASE "v"
        verts(vIdx).x = VAL(val1)
        verts(vIdx).y = VAL(val2)
        verts(vIdx).z = VAL(val3)
        vIdx = vIdx + 1
      CASE "f"
        model(fIdx).v1 = VAL(val1)
        model(fIdx).v2 = VAL(val2)
        model(fIdx).v3 = VAL(val3)
        fIdx = fIdx + 1
    END SELECT
  LOOP

  modelSize = fIdx
  vertsSize = vIdx
END SUB

' Rotate around the y-axis
' angle in radians
SUB Rotate (angle AS SINGLE, verts() AS Vertex, size AS INTEGER)
  DIM vtx AS Vertex

  FOR i = LBOUND(verts) TO size
    vtx = verts(i)
    verts(i).x = vtx.x * COS(angle) - vtx.z * SIN(angle)
    verts(i).z = vtx.x * SIN(angle) + vtx.z * COS(angle)
  NEXT i

END SUB

SUB TransformWorld (verts() AS Vertex, size AS INTEGER)
  DIM vtx AS Vertex

  FOR i = LBOUND(verts) TO size
    vtx = verts(i)
    verts(i).y = vtx.y - 10
    verts(i).z = vtx.z + 100
  NEXT i

END SUB

