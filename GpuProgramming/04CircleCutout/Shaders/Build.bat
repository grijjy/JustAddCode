"../../Tools/fxc" /T vs_3_0 /E main /O3 /FoVertexShader.DX9 VertexShader.DX.txt
"../../Tools/fxc" /T vs_4_0 /E main /O3 /FoVertexShader.DX11 VertexShader.DX.txt
"../../Tools/fxc" /T ps_3_0 /E main /O3 /FoPixelShader.DX9 PixelShader.DX.txt
"../../Tools/fxc" /T ps_4_0 /E main /O3 /FoPixelShader.DX11 PixelShader.DX.txt
rc Shaders.rc