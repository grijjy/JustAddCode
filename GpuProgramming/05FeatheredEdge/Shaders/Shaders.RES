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
}   @   ��
 P I X E L _ S H A D E R _ D X 9         0 	         ����. CTAB   �    ��       �  |   D         L       \         l       Feather             Sampler+Texture            ps_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111 Q   �   @  ��      �?Q   �   �  @@            �  �     � �    �  �  � U�Z    �  �  � ��    �   �    �   �    �   � ��)  ��   �   � ��*       �   �    �   �  U�    �   �   �    �   �  � U�    �  ��   �    �  U�   �B   �  � �   �   � �X   �  U� � �+   ��  �  @   ��
 P I X E L _ S H A D E R _ D X 1 1       0 	        DXBC�jK��pY"�1N��   �     4   P  �  �  @  RDEF     �          �� �  �   |                             �            ����          �                              Sampler Texture $Globals ����      �              �             �       Feather              Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGNP         8                    D                   SV_POSITION TEXCOORD ���OSGN,                               SV_Target ��SHDR\  @   �   Y  F�         Z   `     X  p     UU  b 2    e  �      h     2  2      F    @     @   @        @    ��  ��                F      F      K        
               
 �A       @    �?1  "      
      @             6  �      @                  >    E  	�     F    F~      `     1  "      
      
�            B      @    �?  �?  �?  �?
�          8        *      
      2  	B      
      @     �@    @@8        
      
      8        
      *      8  �           F    7  	�      V     F    F    >  STATt                
                                                                                              0  <   ��
 P I X E L _ S H A D E R _ G L       0 	        uniform sampler2D _Texture;
uniform float _Feather;

varying vec2 TexCoord;

void main()
{
  // TexCoord ranges from 0.0 to 1.0
  // For calculating a unit circle, remap to -1.0 to 1.0
  vec2 locationFromCenter = (2.0 * TexCoord) - vec2(1.0, 1.0);
  
  // Calculate distance from center, but subtract it from 1.0 so 0.0 is at
  // the edge, and 1.0 is at the center. This makes subsequent calculations
  // easier.
  float distance = 1.0 - length(locationFromCenter);
  
  // A distance less than 0 means we are outside the circle.
  // Return a transparent color in that case
  if (distance < 0.0)
  {
    gl_FragColor = vec4(0.0, 0.0, 0.0, 0.0);
    return;
  }
    
  // Get pixel color from texture
  vec4 color = texture2D(_Texture, TexCoord);
  
  // If the distance is between 0.0 and Feather, then smoothly interpolate
  // the Alpha value so the edge fades out to fully transparent
  if (distance < _Feather)
  {
    float alpha = smoothstep(0.0, _Feather, distance);
    color = color * alpha;
  }
  
  gl_FragColor = color;
}   @   ��
 P I X E L _ S H A D E R _ M T L         0 	        using namespace metal;

struct ProjectedVertex
{
  float4 position [[position]];
  float2 texCoord;
};

fragment float4 fragmentShader(
  const ProjectedVertex in [[stage_in]],
  constant float &Feather [[buffer(0)]],
  const texture2d<float> Image [[texture(0)]],
  const sampler ImageSampler [[sampler(0)]])
{
  // texCoord ranges from 0.0 to 1.0
  // For calculating a unit circle, remap to -1.0 to 1.0
  float2 locationFromCenter = (2.0 * in.texCoord) - float2(1.0, 1.0);
  
  // Calculate distance from center, but subtract it from 1.0 so 0.0 is at
  // the edge, and 1.0 is at the center. This makes subsequent calculations
  // easier.
  float distance = 1.0 - length(locationFromCenter);
  
  // A distance less than 0 means we are outside the circle.
  // Return a transparent color in that case
  if (distance < 0.0)
    return float4(0.0, 0.0, 0.0, 0.0);
    
  // Get pixel color from texture
  float4 color = Image.sample(ImageSampler, in.texCoord);
  
  // If the distance is between 0.0 and Feather, then smoothly interpolate
  // the Alpha value so the edge fades out to fully transparent
  if (distance < Feather)
  {
    float alpha = smoothstep(0.0, Feather, distance);
    color = color * alpha;
  }
  
  return color;
}