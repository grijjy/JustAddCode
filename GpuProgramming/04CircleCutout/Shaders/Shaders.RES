        ��  ��                    @   ��
 V E R T E X _ S H A D E R _ D X 9       0 	         ����" CTAB   S    ��       �  L   0         <       MVPMatrix ��           vs_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111      �  �    � �     �  �    � �    � �  U�    �  �   �  �    � �  ��  �    � �  ��  �   � ���  8  D   ��
 V E R T E X _ S H A D E R _ D X 1 1         0 	        DXBC�MɎa�^�t�ܳ�   8     4     X  �  �  RDEF�      H          �� �  �   <                              $Globals ���<      `   @           x       @      �       MVPMatrix ��            Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGNL         8                    A                   POSITION TEXCOORD ��OSGNP         8                    D                   SV_Position TEXCOORD ���SHDR  @  A   Y  F�         _  �     _  2    g  �         e  2     h     8  �      V     F�         2  
�      F�               F     2  
�      F�         �     F     2  
�      F�         �     F     6  2     F    >  STATt                                                                                                                 q  @   ��
 V E R T E X _ S H A D E R _ G L         0 	        uniform vec4 _MVPMatrix[4];

attribute vec4 a_Position;
attribute vec2 a_TexCoord0;

varying vec2 TexCoord;

void main()
{
  gl_Position.x = dot(_MVPMatrix[0], a_Position);
  gl_Position.y = dot(_MVPMatrix[1], a_Position);
  gl_Position.z = dot(_MVPMatrix[2], a_Position);
  gl_Position.w = dot(_MVPMatrix[3], a_Position); 
  
  TexCoord = a_TexCoord0;
}     @   ��
 V E R T E X _ S H A D E R _ M T L       0 	        using namespace metal;

struct Vertex 
{
  <#VertexDeclaration#>
};

struct ProjectedVertex
{
  float4 position [[position]];
  float2 texCoord;
};

vertex ProjectedVertex vertexShader(
  constant Vertex *vertexArray [[buffer(0)]],
  const unsigned int vertexId [[vertex_id]],
  constant float4x4 &MVPMatrix [[buffer(1)]]) 
{
  Vertex in = vertexArray[vertexId];
  ProjectedVertex out;
  out.position = float4(in.position[0], in.position[1], in.position[2], 1) * MVPMatrix;
  out.texCoord = in.texcoord0;
  return out;
} 8  @   ��
 P I X E L _ S H A D E R _ D X 9         0 	         ����# CTAB   W    ��       �  P   0         @       Sampler+Texture            ps_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111 Q    �   @  ��      �?    �  �     � �    �  �   �  U�Z    �  �  �  ��    �   �    �   �)   ��   �   �  ��*   B   �  � �+   ��    @   ��
 P I X E L _ S H A D E R _ D X 1 1       0 	        DXBC*�6�랙pq~Ss        4   �   4  h  �  RDEF�                  �� �  l   \                             d            ����          Sampler Texture Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGNP         8                    D                   SV_POSITION TEXCOORD ���OSGN,                               SV_Target ��SHDR(  @   J   Z   `     X  p     UU  b 2    e  �      h     2  2      F    @     @   @        @    ��  ��                F      F      K        
      1        @    �?
       
      6  �      @                  >    E  	�      F    F~      `     >  STATt   
                                                                                                            ]  <   ��
 P I X E L _ S H A D E R _ G L       0 	        uniform sampler2D _Texture;

varying vec2 TexCoord;

void main()
{
  // TexCoord ranges from 0.0 to 1.0
  // For calculating a unit circle, remap to -1.0 to 1.0
  vec2 locationFromCenter = (2.0 * TexCoord) - vec2(1.0, 1.0);
  
  // Calculate distance from center
  float distanceFromCenter = length(locationFromCenter);
  
  // A distance greater than 1 means we are outside the circle.
  // Return a transparent color in that case
  if (distanceFromCenter > 1.0)
  {
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
    return;
  }
    
  gl_FragColor = texture2D(_Texture, TexCoord);
}     @   ��
 P I X E L _ S H A D E R _ M T L         0 	        using namespace metal;

struct ProjectedVertex
{
  float4 position [[position]];
  float2 texCoord;
};

fragment float4 fragmentShader(
  const ProjectedVertex in [[stage_in]],
  const texture2d<float> Image [[texture(0)]],
  const sampler ImageSampler [[sampler(0)]])
{
  // texCoord ranges from 0.0 to 1.0
  // For calculating a unit circle, remap to -1.0 to 1.0
  float2 locationFromCenter = (2.0 * in.texCoord) - float2(1.0, 1.0);
  
  // Calculate distance from center
  float distanceFromCenter = length(locationFromCenter);
  
  // A distance greater than 1 means we are outside the circle.
  // Return a transparent color in that case
  if (distanceFromCenter > 1.0)
    return float4(0.0, 0.0, 0.0, 0.0);

  return Image.sample(ImageSampler, in.texCoord);
}