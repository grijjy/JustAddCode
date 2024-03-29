        ’’  ’’                    @   ’’
 V E R T E X _ S H A D E R _ D X 9       0 	         ž’ž’" CTAB   S    ž’         L   0         <       MVPMatrix ««           vs_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111                    ą     ą     ä   U      ä      ä     ä   Ŗ  ä    ą ä   ’  ä   ą ä’’  8  D   ’’
 V E R T E X _ S H A D E R _ D X 1 1         0 	        DXBCMÉa^ĢtŻÜ³Ž   8     4     X  °  ¼  RDEFČ      H          ž’      <                              $Globals «««<      `   @           x       @             MVPMatrix ««            Microsoft (R) HLSL Shader Compiler 9.29.952.3111 «««ISGNL         8                    A                   POSITION TEXCOORD ««OSGNP         8                    D                   SV_Position TEXCOORD «««SHDR  @  A   Y  F         _  ņ     _  2    g  ņ         e  2     h     8  ņ      V     F         2  
ņ      F               F     2  
ņ      F         ¦     F     2  
ņ      F         ö     F     6  2     F    >  STATt                                                                                                                 e  @   ’’
 V E R T E X _ S H A D E R _ G L         0 	        uniform vec4 _MVPMatrix[4];

attribute vec4 a_Position;
attribute vec2 a_TexCoord0;

varying vec2 UV;

void main()
{
  gl_Position.x = dot(_MVPMatrix[0], a_Position);
  gl_Position.y = dot(_MVPMatrix[1], a_Position);
  gl_Position.z = dot(_MVPMatrix[2], a_Position);
  gl_Position.w = dot(_MVPMatrix[3], a_Position); 
  
  UV = a_TexCoord0;
}     @   ’’
 V E R T E X _ S H A D E R _ M T L       0 	        using namespace metal;

struct Vertex 
{
  <#VertexDeclaration#>
};

struct ProjectedVertex
{
  float4 position [[position]];
  float2 uv;
};

vertex ProjectedVertex vertexShader(
  constant Vertex *vertexArray [[buffer(0)]],
  const unsigned int vertexId [[vertex_id]],
  constant float4x4 &MVPMatrix [[buffer(1)]]) 
{
  Vertex in = vertexArray[vertexId];
  ProjectedVertex out;
  out.position = float4(in.position[0], in.position[1], in.position[2], 1) * MVPMatrix;
  out.uv = in.texcoord0;
  return out;
}   @   ’’
 P I X E L _ S H A D E R _ D X 9         0 	         ’’ž’1 CTAB       ’’            X        h       x        h                h       FireworksCount «            SparkCount Time ps_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111 Q               ĄAQ    ŪÉ@ŪIĄc“”µĻ7Q    ŗ	¶ŗ¤Ŗ*=   æ  ?Q      aD>o:    Q    Evt?“X·?ł">  >0    š’                           &    äš-   ’       ’      ’ ’             U      U      U   ’     ä    ä    U     ä ä    ł Ŗ  ’     ł ä       ł ä U     ł ä Ŗ     ä  ’     ä       ä    ä U       ’     ł      ä Ŗ      ä    U &    äš-  ’        ’ ’    ’ ä  ’     ä    ä    U     ä ä    D Ŗ  ’     D ä       D ä U     D ä Ŗ     ä ī ’     ä       ä    ä Ŗ       ä é    ä  äZ    ä ä U     ’    ä ’ ä    ’ ’ '        ä      ’ ’ '        ä    ’ ’’    @   ’’
 P I X E L _ S H A D E R _ D X 1 1       0 	        DXBCząV¤o@«¢6        4   H     Ō    RDEF     H          ’’   Ś   <                              $Globals «««<      `              Ø             °       Ą            °       Ļ            °       Time «««             FireworksCount SparkCount Microsoft (R) HLSL Shader Compiler 9.29.952.3111 «ISGNP         8                    D                   SV_POSITION TEXCOORD «««OSGN,                               SV_Target ««SHDR4  @     Y  F         b 2    e  ņ      h     6  ņ      @                 Ą0         :                 
     2  
     :      @  A
          A  "     
     8  
r     V    @    Ą@  A  ?    M  r      Š  F    8  
r     F    @    aD  aD  aD      r     F       
r     F    @  >>>             A      
        "     
 A      @    ?8  ā     V    	    8  
ā     V    @      o:o:o:6  r     F     6  B     @      0         *     *           :     8       :      *     8  
2     ö    @    Ą@  A        M  2      Š  F     8  
2     F     @    aD  aD          2     F        
2     F     @     æ   æ        2  
2      A      F     F        2     F A      F           F     F     K       :       r         ö       r     F    F       B     *     @    ?  6  r      F             :      @    ?  6  r      F     6        @    ?>  STATt   (                                                                                                            Ą  <   ’’
 P I X E L _ S H A D E R _ G L       0 	        uniform float _Time;
uniform float _FireworksCount;
uniform float _SparkCount;

varying vec2 UV;

#define random(x) fract(sin(vec4(6.0, 9.0, 1.0 ,0.0) * x) * 9e2) 

void main()
{
  vec4 color = vec4(0.0, 0.0, 0.0, 0.0);
  
  for (float i = -2.0; i < _FireworksCount; i++)
  {
    float fireworkTime = i * 9.1 + _Time;
    float d = floor(fireworkTime);
    vec4 fireworkPosAndColor = random(d) + 0.3;
    fireworkTime -= d;
    for (d = 0.0; d < _SparkCount; d++)
      color += fireworkPosAndColor * (1.0 - fireworkTime) / 1e3 / 
        length(UV - (fireworkPosAndColor - fireworkTime * (random(d * i) - 0.5)).xy);
  }
  
  gl_FragColor = vec4(color.r, color.g, color.b, 1.0);
}  @   ’’
 P I X E L _ S H A D E R _ M T L         0 	        using namespace metal;

struct ProjectedVertex
{
  float4 position [[position]];
  float2 uv;
};

#define random(x) fract(sin(float4(6.0, 9.0, 1.0 ,0.0) * x) * 9e2) 

fragment float4 fragmentShader(
  const ProjectedVertex in [[stage_in]],
  constant float &Time [[buffer(0)]],
  constant float &FireworksCount [[buffer(1)]],
  constant float &SparkCount [[buffer(2)]])
{
  float4 color = float4(0.0, 0.0, 0.0, 0.0);
  for (float i = -2.0; i < FireworksCount; i++)
  {
    float fireworkTime = i * 9.1 + Time;
    float d = floor(fireworkTime);
    float4 fireworkPosAndColor = random(d) + 0.3;
    fireworkTime -= d;
    for (d = 0.0; d < SparkCount; d++)
      color += fireworkPosAndColor * (1.0 - fireworkTime) / 1e3 / 
        length(in.uv - (fireworkPosAndColor - fireworkTime * (random(d * i) - 0.5)).xy);
  }
  
  return float4(color.r, color.g, color.b, 1.0);
} 