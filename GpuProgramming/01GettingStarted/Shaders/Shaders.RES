        ��  ��                  �   @   ��
 V E R T E X _ S H A D E R _ D X 9       0 	         ����" CTAB   S    ��       �  L   0         <       MVPMatrix ��           vs_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111      �  �     �  �    � �  U�    �  �   �  �    � �  ��  �    � �  ��  ���  �  D   ��
 V E R T E X _ S H A D E R _ D X 1 1         0 	        DXBCG�.y���w�v�L:C�   �     4     8  l  L  RDEF�      H          �� �  �   <                              $Globals ���<      `   @           x       @      �       MVPMatrix ��            Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGN,                              POSITION ���OSGN,                              SV_Position SHDR�   @  6   Y  F�         _  �     g  �         h     8  �      V     F�         2  
�      F�               F     2  
�      F�         �     F     2  
�      F�         �     F     >  STATt                                                                                                                    @   ��
 V E R T E X _ S H A D E R _ G L         0 	        uniform vec4 _MVPMatrix[4];

attribute vec4 a_Position;

void main()
{
  gl_Position.x = dot(_MVPMatrix[0], a_Position);
  gl_Position.y = dot(_MVPMatrix[1], a_Position);
  gl_Position.z = dot(_MVPMatrix[2], a_Position);
  gl_Position.w = dot(_MVPMatrix[3], a_Position); 
} �  @   ��
 V E R T E X _ S H A D E R _ M T L       0 	        using namespace metal;

struct Vertex 
{
  <#VertexDeclaration#>
};

struct ProjectedVertex
{
  float4 position [[position]];
};

vertex ProjectedVertex vertexShader(
  constant Vertex *vertexArray [[buffer(0)]],
  const unsigned int vertexId [[vertex_id]],
  constant float4x4 &MVPMatrix [[buffer(1)]]) 
{
  Vertex in = vertexArray[vertexId];
  ProjectedVertex out;
  out.position = float4(in.position[0], in.position[1], in.position[2], 1) * MVPMatrix;
  return out;
} �   @   ��
 P I X E L _ S H A D E R _ D X 9         0 	         ���� CTAB   #    ��         �     ps_3_0 Microsoft (R) HLSL Shader Compiler 9.29.952.3111 Q    �      �?           �  P���  �  @   ��
 P I X E L _ S H A D E R _ D X 1 1       0 	        DXBC�fUQ��� ����   �     4   �   �   �     RDEFP                   �� �     Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGN          OSGN,                               SV_Target ��SHDR8   @      e  �      6  �      @            �?  �?>  STATt                                                                                                                   =   <   ��
 P I X E L _ S H A D E R _ G L       0 	        void main()
{
  gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
}   P   @   ��
 P I X E L _ S H A D E R _ M T L         0 	        fragment float4 fragmentShader()
{
  return float4(0.0, 0.0, 1.0, 1.0);    
}