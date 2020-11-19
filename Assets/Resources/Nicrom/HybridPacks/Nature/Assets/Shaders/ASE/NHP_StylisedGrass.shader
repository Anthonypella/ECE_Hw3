// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Nicrom/NHP/ASE/Stylised Grass"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[ASEBegin][NoScaleOffset]_MainTex("Main Tex", 2D) = "white" {}
		_Color1("Color 1", Color) = (0.3393185,0.490566,0.09255961,1)
		_Color2("Color 2", Color) = (0.719914,0.8207547,0.3639195,1)
		_ColorBlendStart("Color Blend Start", Range( 0 , 1)) = 0
		_ColorBlendEnd("Color Blend End", Range( 0 , 1)) = 1
		_AlphaCutoff("Alpha Cutoff", Range( 0 , 1)) = 0.5
		_MBDefaultBending("MB Default Bending", Float) = 0
		_MBAmplitude("MB Amplitude", Float) = 1.5
		_MBAmplitudeOffset("MB Amplitude Offset", Float) = 2
		_MBFrequency("MB Frequency", Float) = 1.11
		_MBFrequencyOffset("MB Frequency Offset", Float) = 0
		_MBWindDir("MB Wind Dir", Range( 0 , 360)) = 0
		_MBWindDirOffset("MB Wind Dir Offset", Range( 0 , 180)) = 20
		_MBWindDirBlend("MB Wind Dir Blend", Range( 0 , 1)) = 0
		_MBPhase("MB Phase", Float) = 3
		_MBMaxHeight("MB Max Height", Float) = 1
		[Toggle(_ENABLESLOPECORRECTION_ON)] _EnableSlopeCorrection("Enable Slope Correction", Float) = 1
		_SlopeCorrectionMagnitude("Slope Correction Magnitude", Range( 0 , 1)) = 1
		_SlopeCorrectionOffset("Slope Correction Offset", Range( 0 , 1)) = 0
		[NoScaleOffset]_NoiseTexture("Noise Texture", 2D) = "white" {}
		_NoiseTextureTilling("Noise Tilling - Static (XY), Animated (ZW)", Vector) = (1,1,1,1)
		[ASEEnd]_NoisePannerSpeed("Noise Panner Speed", Vector) = (0.05,0.03,0,0)
		[HideInInspector] _texcoord( "", 2D ) = "white" {}

		//_TransmissionShadow( "Transmission Shadow", Range( 0, 1 ) ) = 0.5
		//_TransStrength( "Trans Strength", Range( 0, 50 ) ) = 1
		//_TransNormal( "Trans Normal Distortion", Range( 0, 1 ) ) = 0.5
		//_TransScattering( "Trans Scattering", Range( 1, 50 ) ) = 2
		//_TransDirect( "Trans Direct", Range( 0, 1 ) ) = 0.9
		//_TransAmbient( "Trans Ambient", Range( 0, 1 ) ) = 0.1
		//_TransShadow( "Trans Shadow", Range( 0, 1 ) ) = 0.5
		//_TessPhongStrength( "Tess Phong Strength", Range( 0, 1 ) ) = 0.5
		//_TessValue( "Tess Max Tessellation", Range( 1, 32 ) ) = 16
		//_TessMin( "Tess Min Distance", Float ) = 10
		//_TessMax( "Tess Max Distance", Float ) = 25
		//_TessEdgeLength ( "Tess Edge length", Range( 2, 50 ) ) = 16
		//_TessMaxDisp( "Tess Max Displacement", Float ) = 25
	}

	SubShader
	{
		LOD 0

		

		Tags { "RenderPipeline"="UniversalPipeline" "RenderType"="Opaque" "Queue"="Geometry" }
		Cull Off
		AlphaToMask On
		HLSLINCLUDE
		#pragma target 3.0

		float4 FixedTess( float tessValue )
		{
			return tessValue;
		}
		
		float CalcDistanceTessFactor (float4 vertex, float minDist, float maxDist, float tess, float4x4 o2w, float3 cameraPos )
		{
			float3 wpos = mul(o2w,vertex).xyz;
			float dist = distance (wpos, cameraPos);
			float f = clamp(1.0 - (dist - minDist) / (maxDist - minDist), 0.01, 1.0) * tess;
			return f;
		}

		float4 CalcTriEdgeTessFactors (float3 triVertexFactors)
		{
			float4 tess;
			tess.x = 0.5 * (triVertexFactors.y + triVertexFactors.z);
			tess.y = 0.5 * (triVertexFactors.x + triVertexFactors.z);
			tess.z = 0.5 * (triVertexFactors.x + triVertexFactors.y);
			tess.w = (triVertexFactors.x + triVertexFactors.y + triVertexFactors.z) / 3.0f;
			return tess;
		}

		float CalcEdgeTessFactor (float3 wpos0, float3 wpos1, float edgeLen, float3 cameraPos, float4 scParams )
		{
			float dist = distance (0.5 * (wpos0+wpos1), cameraPos);
			float len = distance(wpos0, wpos1);
			float f = max(len * scParams.y / (edgeLen * dist), 1.0);
			return f;
		}

		float DistanceFromPlane (float3 pos, float4 plane)
		{
			float d = dot (float4(pos,1.0f), plane);
			return d;
		}

		bool WorldViewFrustumCull (float3 wpos0, float3 wpos1, float3 wpos2, float cullEps, float4 planes[6] )
		{
			float4 planeTest;
			planeTest.x = (( DistanceFromPlane(wpos0, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[0]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[0]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.y = (( DistanceFromPlane(wpos0, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[1]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[1]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.z = (( DistanceFromPlane(wpos0, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[2]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[2]) > -cullEps) ? 1.0f : 0.0f );
			planeTest.w = (( DistanceFromPlane(wpos0, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos1, planes[3]) > -cullEps) ? 1.0f : 0.0f ) +
						  (( DistanceFromPlane(wpos2, planes[3]) > -cullEps) ? 1.0f : 0.0f );
			return !all (planeTest);
		}

		float4 DistanceBasedTess( float4 v0, float4 v1, float4 v2, float tess, float minDist, float maxDist, float4x4 o2w, float3 cameraPos )
		{
			float3 f;
			f.x = CalcDistanceTessFactor (v0,minDist,maxDist,tess,o2w,cameraPos);
			f.y = CalcDistanceTessFactor (v1,minDist,maxDist,tess,o2w,cameraPos);
			f.z = CalcDistanceTessFactor (v2,minDist,maxDist,tess,o2w,cameraPos);

			return CalcTriEdgeTessFactors (f);
		}

		float4 EdgeLengthBasedTess( float4 v0, float4 v1, float4 v2, float edgeLength, float4x4 o2w, float3 cameraPos, float4 scParams )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;
			tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
			tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
			tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
			tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			return tess;
		}

		float4 EdgeLengthBasedTessCull( float4 v0, float4 v1, float4 v2, float edgeLength, float maxDisplacement, float4x4 o2w, float3 cameraPos, float4 scParams, float4 planes[6] )
		{
			float3 pos0 = mul(o2w,v0).xyz;
			float3 pos1 = mul(o2w,v1).xyz;
			float3 pos2 = mul(o2w,v2).xyz;
			float4 tess;

			if (WorldViewFrustumCull(pos0, pos1, pos2, maxDisplacement, planes))
			{
				tess = 0.0f;
			}
			else
			{
				tess.x = CalcEdgeTessFactor (pos1, pos2, edgeLength, cameraPos, scParams);
				tess.y = CalcEdgeTessFactor (pos2, pos0, edgeLength, cameraPos, scParams);
				tess.z = CalcEdgeTessFactor (pos0, pos1, edgeLength, cameraPos, scParams);
				tess.w = (tess.x + tess.y + tess.z) / 3.0f;
			}
			return tess;
		}
		ENDHLSL

		
		Pass
		{
			
			Name "Forward"
			Tags { "LightMode"="UniversalForward" }
			
			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA
			

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 70301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS
			#pragma multi_compile _ _MAIN_LIGHT_SHADOWS_CASCADE
			#pragma multi_compile _ _ADDITIONAL_LIGHTS_VERTEX _ADDITIONAL_LIGHTS
			#pragma multi_compile _ _ADDITIONAL_LIGHT_SHADOWS
			#pragma multi_compile _ _SHADOWS_SOFT
			#pragma multi_compile _ _MIXED_LIGHTING_SUBTRACTIVE
			
			#pragma multi_compile _ DIRLIGHTMAP_COMBINED
			#pragma multi_compile _ LIGHTMAP_ON

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_FORWARD

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#if ASE_SRP_VERSION <= 70108
			#define REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR
			#endif

			#if defined(UNITY_INSTANCING_ENABLED) && defined(_TERRAIN_INSTANCED_PERPIXEL_NORMAL)
			    #define ENABLE_TERRAIN_PERPIXEL_NORMAL
			#endif

			#define ASE_NEEDS_VERT_POSITION
			#pragma shader_feature_local _ENABLESLOPECORRECTION_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord : TEXCOORD0;
				float4 ase_color : COLOR;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				float4 lightmapUVOrVertexSH : TEXCOORD0;
				half4 fogFactorAndVertexLight : TEXCOORD1;
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				float4 shadowCoord : TEXCOORD2;
				#endif
				float4 tSpace0 : TEXCOORD3;
				float4 tSpace1 : TEXCOORD4;
				float4 tSpace2 : TEXCOORD5;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 screenPos : TEXCOORD6;
				#endif
				float4 ase_texcoord7 : TEXCOORD7;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _NoiseTextureTilling;
			float4 _Color2;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _ColorBlendEnd;
			float _AlphaCutoff;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			float MBGlobalWindDir;
			sampler2D _NoiseTexture;
			sampler2D _MainTex;
			SAMPLER(sampler_MainTex);


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float lerpResult133 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection149 = lerpResult133;
				float MB_WindDirectionOffset146 = _MBWindDirOffset;
				float4 transform1_g173 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord110 = v.texcoord1.xyzw.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult111 = (float3(texCoord110.x , 0.0 , texCoord110.y));
				float3 Pivot113 = -appendResult111;
				float4 transform2_g173 = mul(GetObjectToWorldMatrix(),float4( Pivot113 , 0.0 ));
				float2 UVs27_g174 = ( (transform1_g173).xz + (transform2_g173).xz );
				float4 temp_output_24_0_g174 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g174 = (temp_output_24_0_g174).zw;
				float2 panner7_g174 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldSpaceNoise147 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g174 * AnimatedNoiseTilling29_g174 ) + panner7_g174 ), 0, 0.0) );
				float temp_output_11_0_g178 = radians( ( ( MB_WindDirection149 + ( MB_WindDirectionOffset146 * (-1.0 + ((AnimatedWorldSpaceNoise147).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g178 = (float3(cos( temp_output_11_0_g178 ) , 0.0 , sin( temp_output_11_0_g178 )));
				float4 transform15_g178 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g178 , 0.0 ));
				float3 normalizeResult34_g178 = normalize( (transform15_g178).xyz );
				float3 MB_RotationAxis164 = normalizeResult34_g178;
				float MB_Amplitude145 = _MBAmplitude;
				float MB_AmplitudeOffset143 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g174 = (temp_output_24_0_g174).xy;
				float4 StaticWorldSpaceNoise148 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g174 * StaticNoileTilling28_g174 ), 0, 0.0) );
				float4 StaticWorldNoise31_g179 = StaticWorldSpaceNoise148;
				float4 transform8_g179 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency141 = _MBFrequency;
				float MB_FrequencyOffset138 = _MBFrequencyOffset;
				float DB_PhaseShift140 = v.ase_color.a;
				float MB_Phase144 = _MBPhase;
				float MB_DefaultBending137 = _MBDefaultBending;
				float MB_MaxHeight139 = _MBMaxHeight;
				float MB_RotationAngle166 = radians( ( ( ( ( MB_Amplitude145 + ( MB_AmplitudeOffset143 * (StaticWorldNoise31_g179).x ) ) * sin( ( ( ( transform8_g179.x + transform8_g179.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency141 + ( MB_FrequencyOffset138 * (StaticWorldNoise31_g179).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift140 ) ) ) * MB_Phase144 ) ) ) + MB_DefaultBending137 ) * ( v.vertex.xyz.y / MB_MaxHeight139 ) ) );
				float3 appendResult167 = (float3(v.vertex.xyz.x , 0.0 , v.vertex.xyz.z));
				float3 rotatedValue175 = RotateAroundAxis( appendResult167, v.vertex.xyz, MB_RotationAxis164, MB_RotationAngle166 );
				float3 temp_output_190_0 = ( ( rotatedValue175 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g180 = temp_output_190_0;
				float3 appendResult15_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g180 , 0.0 ));
				float4 break20_g180 = transform17_g180;
				float3 appendResult24_g180 = (float3(-break20_g180.z , 0.0 , break20_g180.x));
				float3 appendResult3_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g180 , 0.0 ));
				float3 lerpResult84_g180 = lerp( float3(0,1,0) , (transform4_g180).xyz , step( 1E-06 , ( abs( transform4_g180.x ) + abs( transform4_g180.z ) ) ));
				float3 normalizeResult7_g180 = normalize( lerpResult84_g180 );
				float dotResult9_g180 = dot( normalizeResult7_g180 , float3(0,1,0) );
				float temp_output_12_0_g180 = acos( dotResult9_g180 );
				float NaNPrevention21_g180 = step( 0.01 , abs( ( temp_output_12_0_g180 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g180 = lerp( float3(1,0,0) , appendResult24_g180 , NaNPrevention21_g180);
				float4 transform28_g180 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g180 , 0.0 ));
				float3 normalizeResult49_g180 = normalize( (transform28_g180).xyz );
				float3 RotationAxis30_g180 = normalizeResult49_g180;
				float SlopeCorrectionOffset191 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude189 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g180 = ( saturate( ( (0.0 + ((StaticWorldSpaceNoise148).x - 0.0) * (SlopeCorrectionOffset191 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude189 ) ) * temp_output_12_0_g180 );
				float3 rotatedValue35_g180 = RotateAroundAxis( Pivot113, ( v.vertex.xyz + MainBending89_g180 ), RotationAxis30_g180, RotationAngle29_g180 );
				float3 lerpResult52_g180 = lerp( MainBending89_g180 , ( rotatedValue35_g180 - v.vertex.xyz ) , NaNPrevention21_g180);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch203 = lerpResult52_g180;
				#else
				float3 staticSwitch203 = temp_output_190_0;
				#endif
				float3 LocalVertexOffset206 = staticSwitch203;
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset206;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif
				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float3 positionVS = TransformWorldToView( positionWS );
				float4 positionCS = TransformWorldToHClip( positionWS );

				VertexNormalInputs normalInput = GetVertexNormalInputs( v.ase_normal, v.ase_tangent );

				o.tSpace0 = float4( normalInput.normalWS, positionWS.x);
				o.tSpace1 = float4( normalInput.tangentWS, positionWS.y);
				o.tSpace2 = float4( normalInput.bitangentWS, positionWS.z);

				OUTPUT_LIGHTMAP_UV( v.texcoord1, unity_LightmapST, o.lightmapUVOrVertexSH.xy );
				OUTPUT_SH( normalInput.normalWS.xyz, o.lightmapUVOrVertexSH.xyz );

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					o.lightmapUVOrVertexSH.zw = v.texcoord;
					o.lightmapUVOrVertexSH.xy = v.texcoord * unity_LightmapST.xy + unity_LightmapST.zw;
				#endif

				half3 vertexLight = VertexLighting( positionWS, normalInput.normalWS );
				#ifdef ASE_FOG
					half fogFactor = ComputeFogFactor( positionCS.z );
				#else
					half fogFactor = 0;
				#endif
				o.fogFactorAndVertexLight = half4(fogFactor, vertexLight);
				
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
				VertexPositionInputs vertexInput = (VertexPositionInputs)0;
				vertexInput.positionWS = positionWS;
				vertexInput.positionCS = positionCS;
				o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				
				o.clipPos = positionCS;
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				o.screenPos = ComputeScreenPos(positionCS);
				#endif
				return o;
			}
			
			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_tangent : TANGENT;
				float4 texcoord : TEXCOORD0;
				float4 texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_tangent = v.ase_tangent;
				o.texcoord = v.texcoord;
				o.texcoord1 = v.texcoord1;
				o.ase_color = v.ase_color;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_tangent = patch[0].ase_tangent * bary.x + patch[1].ase_tangent * bary.y + patch[2].ase_tangent * bary.z;
				o.texcoord = patch[0].texcoord * bary.x + patch[1].texcoord * bary.y + patch[2].texcoord * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag ( VertexOutput IN  ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(IN);

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif

				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float2 sampleCoords = (IN.lightmapUVOrVertexSH.zw / _TerrainHeightmapRecipSize.zw + 0.5f) * _TerrainHeightmapRecipSize.xy;
					float3 WorldNormal = TransformObjectToWorldNormal(normalize(SAMPLE_TEXTURE2D(_TerrainNormalmapTexture, sampler_TerrainNormalmapTexture, sampleCoords).rgb * 2 - 1));
					float3 WorldTangent = -cross(GetObjectToWorldMatrix()._13_23_33, WorldNormal);
					float3 WorldBiTangent = cross(WorldNormal, -WorldTangent);
				#else
					float3 WorldNormal = normalize( IN.tSpace0.xyz );
					float3 WorldTangent = IN.tSpace1.xyz;
					float3 WorldBiTangent = IN.tSpace2.xyz;
				#endif
				float3 WorldPosition = float3(IN.tSpace0.w,IN.tSpace1.w,IN.tSpace2.w);
				float3 WorldViewDirection = _WorldSpaceCameraPos.xyz  - WorldPosition;
				float4 ShadowCoords = float4( 0, 0, 0, 0 );
				#if defined(ASE_NEEDS_FRAG_SCREEN_POSITION)
				float4 ScreenPos = IN.screenPos;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
					ShadowCoords = IN.shadowCoord;
				#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
					ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
				#endif
	
				WorldViewDirection = SafeNormalize( WorldViewDirection );

				float2 uv_MainTex187 = IN.ase_texcoord7.xy;
				float4 tex2DNode187 = tex2D( _MainTex, uv_MainTex187 );
				float MainTextureColor193 = tex2DNode187.r;
				float2 texCoord172 = IN.ase_texcoord7.xy * float2( 1,1 ) + float2( 0,0 );
				float smoothstepResult179 = smoothstep( _ColorBlendStart , _ColorBlendEnd , texCoord172.y);
				float4 lerpResult188 = lerp( _Color2 , _Color1 , smoothstepResult179);
				float4 Color194 = lerpResult188;
				float4 Albedo204 = ( MainTextureColor193 * Color194 );
				
				float Opacity205 = tex2DNode187.a;
				
				float3 Albedo = Albedo204.rgb;
				float3 Normal = float3(0, 0, 1);
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = 0.0;
				float Smoothness = 0.0;
				float Occlusion = 1;
				float Alpha = Opacity205;
				float AlphaClipThreshold = _AlphaCutoff;
				float AlphaClipThresholdShadow = 0.5;
				float3 BakedGI = 0;
				float3 RefractionColor = 1;
				float RefractionIndex = 1;
				float3 Transmission = 1;
				float3 Translucency = 1;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				InputData inputData;
				inputData.positionWS = WorldPosition;
				inputData.viewDirectionWS = WorldViewDirection;
				inputData.shadowCoord = ShadowCoords;

				#ifdef _NORMALMAP
					#if _NORMAL_DROPOFF_TS
					inputData.normalWS = TransformTangentToWorld(Normal, half3x3( WorldTangent, WorldBiTangent, WorldNormal ));
					#elif _NORMAL_DROPOFF_OS
					inputData.normalWS = TransformObjectToWorldNormal(Normal);
					#elif _NORMAL_DROPOFF_WS
					inputData.normalWS = Normal;
					#endif
					inputData.normalWS = NormalizeNormalPerPixel(inputData.normalWS);
				#else
					inputData.normalWS = WorldNormal;
				#endif

				#ifdef ASE_FOG
					inputData.fogCoord = IN.fogFactorAndVertexLight.x;
				#endif

				inputData.vertexLighting = IN.fogFactorAndVertexLight.yzw;
				#if defined(ENABLE_TERRAIN_PERPIXEL_NORMAL)
					float3 SH = SampleSH(inputData.normalWS.xyz);
				#else
					float3 SH = IN.lightmapUVOrVertexSH.xyz;
				#endif

				inputData.bakedGI = SAMPLE_GI( IN.lightmapUVOrVertexSH.xy, SH, inputData.normalWS );
				#ifdef _ASE_BAKEDGI
					inputData.bakedGI = BakedGI;
				#endif
				half4 color = UniversalFragmentPBR(
					inputData, 
					Albedo, 
					Metallic, 
					Specular, 
					Smoothness, 
					Occlusion, 
					Emission, 
					Alpha);

				#ifdef _TRANSMISSION_ASE
				{
					float shadow = _TransmissionShadow;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );
					half3 mainTransmission = max(0 , -dot(inputData.normalWS, mainLight.direction)) * mainAtten * Transmission;
					color.rgb += Albedo * mainTransmission;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 transmission = max(0 , -dot(inputData.normalWS, light.direction)) * atten * Transmission;
							color.rgb += Albedo * transmission;
						}
					#endif
				}
				#endif

				#ifdef _TRANSLUCENCY_ASE
				{
					float shadow = _TransShadow;
					float normal = _TransNormal;
					float scattering = _TransScattering;
					float direct = _TransDirect;
					float ambient = _TransAmbient;
					float strength = _TransStrength;

					Light mainLight = GetMainLight( inputData.shadowCoord );
					float3 mainAtten = mainLight.color * mainLight.distanceAttenuation;
					mainAtten = lerp( mainAtten, mainAtten * mainLight.shadowAttenuation, shadow );

					half3 mainLightDir = mainLight.direction + inputData.normalWS * normal;
					half mainVdotL = pow( saturate( dot( inputData.viewDirectionWS, -mainLightDir ) ), scattering );
					half3 mainTranslucency = mainAtten * ( mainVdotL * direct + inputData.bakedGI * ambient ) * Translucency;
					color.rgb += Albedo * mainTranslucency * strength;

					#ifdef _ADDITIONAL_LIGHTS
						int transPixelLightCount = GetAdditionalLightsCount();
						for (int i = 0; i < transPixelLightCount; ++i)
						{
							Light light = GetAdditionalLight(i, inputData.positionWS);
							float3 atten = light.color * light.distanceAttenuation;
							atten = lerp( atten, atten * light.shadowAttenuation, shadow );

							half3 lightDir = light.direction + inputData.normalWS * normal;
							half VdotL = pow( saturate( dot( inputData.viewDirectionWS, -lightDir ) ), scattering );
							half3 translucency = atten * ( VdotL * direct + inputData.bakedGI * ambient ) * Translucency;
							color.rgb += Albedo * translucency * strength;
						}
					#endif
				}
				#endif

				#ifdef _REFRACTION_ASE
					float4 projScreenPos = ScreenPos / ScreenPos.w;
					float3 refractionOffset = ( RefractionIndex - 1.0 ) * mul( UNITY_MATRIX_V, WorldNormal ).xyz * ( 1.0 - dot( WorldNormal, WorldViewDirection ) );
					projScreenPos.xy += refractionOffset.xy;
					float3 refraction = SHADERGRAPH_SAMPLE_SCENE_COLOR( projScreenPos ) * RefractionColor;
					color.rgb = lerp( refraction, color.rgb, color.a );
					color.a = 1;
				#endif

				#ifdef ASE_FINAL_COLOR_ALPHA_MULTIPLY
					color.rgb *= color.a;
				#endif

				#ifdef ASE_FOG
					#ifdef TERRAIN_SPLAT_ADDPASS
						color.rgb = MixFogColor(color.rgb, half3( 0, 0, 0 ), IN.fogFactorAndVertexLight.x );
					#else
						color.rgb = MixFog(color.rgb, IN.fogFactorAndVertexLight.x);
					#endif
				#endif
				
				return color;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "ShadowCaster"
			Tags { "LightMode"="ShadowCaster" }

			ZWrite On
			ZTest LEqual
			AlphaToMask Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 70301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_SHADOWCASTER

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#pragma shader_feature_local _ENABLESLOPECORRECTION_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _NoiseTextureTilling;
			float4 _Color2;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _ColorBlendEnd;
			float _AlphaCutoff;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			float MBGlobalWindDir;
			sampler2D _NoiseTexture;
			sampler2D _MainTex;
			SAMPLER(sampler_MainTex);


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			float3 _LightDirection;

			VertexOutput VertexFunction( VertexInput v )
			{
				VertexOutput o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float lerpResult133 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection149 = lerpResult133;
				float MB_WindDirectionOffset146 = _MBWindDirOffset;
				float4 transform1_g173 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord110 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult111 = (float3(texCoord110.x , 0.0 , texCoord110.y));
				float3 Pivot113 = -appendResult111;
				float4 transform2_g173 = mul(GetObjectToWorldMatrix(),float4( Pivot113 , 0.0 ));
				float2 UVs27_g174 = ( (transform1_g173).xz + (transform2_g173).xz );
				float4 temp_output_24_0_g174 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g174 = (temp_output_24_0_g174).zw;
				float2 panner7_g174 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldSpaceNoise147 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g174 * AnimatedNoiseTilling29_g174 ) + panner7_g174 ), 0, 0.0) );
				float temp_output_11_0_g178 = radians( ( ( MB_WindDirection149 + ( MB_WindDirectionOffset146 * (-1.0 + ((AnimatedWorldSpaceNoise147).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g178 = (float3(cos( temp_output_11_0_g178 ) , 0.0 , sin( temp_output_11_0_g178 )));
				float4 transform15_g178 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g178 , 0.0 ));
				float3 normalizeResult34_g178 = normalize( (transform15_g178).xyz );
				float3 MB_RotationAxis164 = normalizeResult34_g178;
				float MB_Amplitude145 = _MBAmplitude;
				float MB_AmplitudeOffset143 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g174 = (temp_output_24_0_g174).xy;
				float4 StaticWorldSpaceNoise148 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g174 * StaticNoileTilling28_g174 ), 0, 0.0) );
				float4 StaticWorldNoise31_g179 = StaticWorldSpaceNoise148;
				float4 transform8_g179 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency141 = _MBFrequency;
				float MB_FrequencyOffset138 = _MBFrequencyOffset;
				float DB_PhaseShift140 = v.ase_color.a;
				float MB_Phase144 = _MBPhase;
				float MB_DefaultBending137 = _MBDefaultBending;
				float MB_MaxHeight139 = _MBMaxHeight;
				float MB_RotationAngle166 = radians( ( ( ( ( MB_Amplitude145 + ( MB_AmplitudeOffset143 * (StaticWorldNoise31_g179).x ) ) * sin( ( ( ( transform8_g179.x + transform8_g179.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency141 + ( MB_FrequencyOffset138 * (StaticWorldNoise31_g179).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift140 ) ) ) * MB_Phase144 ) ) ) + MB_DefaultBending137 ) * ( v.vertex.xyz.y / MB_MaxHeight139 ) ) );
				float3 appendResult167 = (float3(v.vertex.xyz.x , 0.0 , v.vertex.xyz.z));
				float3 rotatedValue175 = RotateAroundAxis( appendResult167, v.vertex.xyz, MB_RotationAxis164, MB_RotationAngle166 );
				float3 temp_output_190_0 = ( ( rotatedValue175 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g180 = temp_output_190_0;
				float3 appendResult15_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g180 , 0.0 ));
				float4 break20_g180 = transform17_g180;
				float3 appendResult24_g180 = (float3(-break20_g180.z , 0.0 , break20_g180.x));
				float3 appendResult3_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g180 , 0.0 ));
				float3 lerpResult84_g180 = lerp( float3(0,1,0) , (transform4_g180).xyz , step( 1E-06 , ( abs( transform4_g180.x ) + abs( transform4_g180.z ) ) ));
				float3 normalizeResult7_g180 = normalize( lerpResult84_g180 );
				float dotResult9_g180 = dot( normalizeResult7_g180 , float3(0,1,0) );
				float temp_output_12_0_g180 = acos( dotResult9_g180 );
				float NaNPrevention21_g180 = step( 0.01 , abs( ( temp_output_12_0_g180 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g180 = lerp( float3(1,0,0) , appendResult24_g180 , NaNPrevention21_g180);
				float4 transform28_g180 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g180 , 0.0 ));
				float3 normalizeResult49_g180 = normalize( (transform28_g180).xyz );
				float3 RotationAxis30_g180 = normalizeResult49_g180;
				float SlopeCorrectionOffset191 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude189 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g180 = ( saturate( ( (0.0 + ((StaticWorldSpaceNoise148).x - 0.0) * (SlopeCorrectionOffset191 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude189 ) ) * temp_output_12_0_g180 );
				float3 rotatedValue35_g180 = RotateAroundAxis( Pivot113, ( v.vertex.xyz + MainBending89_g180 ), RotationAxis30_g180, RotationAngle29_g180 );
				float3 lerpResult52_g180 = lerp( MainBending89_g180 , ( rotatedValue35_g180 - v.vertex.xyz ) , NaNPrevention21_g180);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch203 = lerpResult52_g180;
				#else
				float3 staticSwitch203 = temp_output_190_0;
				#endif
				float3 LocalVertexOffset206 = staticSwitch203;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset206;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif
				float3 normalWS = TransformObjectToWorldDir(v.ase_normal);

				float4 clipPos = TransformWorldToHClip( ApplyShadowBias( positionWS, normalWS, _LightDirection ) );

				#if UNITY_REVERSED_Z
					clipPos.z = min(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#else
					clipPos.z = max(clipPos.z, clipPos.w * UNITY_NEAR_CLIP_VALUE);
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = clipPos;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID( IN );
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );
				
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex187 = IN.ase_texcoord2.xy;
				float4 tex2DNode187 = tex2D( _MainTex, uv_MainTex187 );
				float Opacity205 = tex2DNode187.a;
				
				float Alpha = Opacity205;
				float AlphaClipThreshold = _AlphaCutoff;
				float AlphaClipThresholdShadow = 0.5;

				#ifdef _ALPHATEST_ON
					#ifdef _ALPHATEST_SHADOW_ON
						clip(Alpha - AlphaClipThresholdShadow);
					#else
						clip(Alpha - AlphaClipThreshold);
					#endif
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}

			ENDHLSL
		}

		
		Pass
		{
			
			Name "DepthOnly"
			Tags { "LightMode"="DepthOnly" }

			ZWrite On
			ColorMask 0
			AlphaToMask Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 70301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_DEPTHONLY

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#pragma shader_feature_local _ENABLESLOPECORRECTION_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _NoiseTextureTilling;
			float4 _Color2;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _ColorBlendEnd;
			float _AlphaCutoff;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			float MBGlobalWindDir;
			sampler2D _NoiseTexture;
			sampler2D _MainTex;
			SAMPLER(sampler_MainTex);


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float lerpResult133 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection149 = lerpResult133;
				float MB_WindDirectionOffset146 = _MBWindDirOffset;
				float4 transform1_g173 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord110 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult111 = (float3(texCoord110.x , 0.0 , texCoord110.y));
				float3 Pivot113 = -appendResult111;
				float4 transform2_g173 = mul(GetObjectToWorldMatrix(),float4( Pivot113 , 0.0 ));
				float2 UVs27_g174 = ( (transform1_g173).xz + (transform2_g173).xz );
				float4 temp_output_24_0_g174 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g174 = (temp_output_24_0_g174).zw;
				float2 panner7_g174 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldSpaceNoise147 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g174 * AnimatedNoiseTilling29_g174 ) + panner7_g174 ), 0, 0.0) );
				float temp_output_11_0_g178 = radians( ( ( MB_WindDirection149 + ( MB_WindDirectionOffset146 * (-1.0 + ((AnimatedWorldSpaceNoise147).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g178 = (float3(cos( temp_output_11_0_g178 ) , 0.0 , sin( temp_output_11_0_g178 )));
				float4 transform15_g178 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g178 , 0.0 ));
				float3 normalizeResult34_g178 = normalize( (transform15_g178).xyz );
				float3 MB_RotationAxis164 = normalizeResult34_g178;
				float MB_Amplitude145 = _MBAmplitude;
				float MB_AmplitudeOffset143 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g174 = (temp_output_24_0_g174).xy;
				float4 StaticWorldSpaceNoise148 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g174 * StaticNoileTilling28_g174 ), 0, 0.0) );
				float4 StaticWorldNoise31_g179 = StaticWorldSpaceNoise148;
				float4 transform8_g179 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency141 = _MBFrequency;
				float MB_FrequencyOffset138 = _MBFrequencyOffset;
				float DB_PhaseShift140 = v.ase_color.a;
				float MB_Phase144 = _MBPhase;
				float MB_DefaultBending137 = _MBDefaultBending;
				float MB_MaxHeight139 = _MBMaxHeight;
				float MB_RotationAngle166 = radians( ( ( ( ( MB_Amplitude145 + ( MB_AmplitudeOffset143 * (StaticWorldNoise31_g179).x ) ) * sin( ( ( ( transform8_g179.x + transform8_g179.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency141 + ( MB_FrequencyOffset138 * (StaticWorldNoise31_g179).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift140 ) ) ) * MB_Phase144 ) ) ) + MB_DefaultBending137 ) * ( v.vertex.xyz.y / MB_MaxHeight139 ) ) );
				float3 appendResult167 = (float3(v.vertex.xyz.x , 0.0 , v.vertex.xyz.z));
				float3 rotatedValue175 = RotateAroundAxis( appendResult167, v.vertex.xyz, MB_RotationAxis164, MB_RotationAngle166 );
				float3 temp_output_190_0 = ( ( rotatedValue175 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g180 = temp_output_190_0;
				float3 appendResult15_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g180 , 0.0 ));
				float4 break20_g180 = transform17_g180;
				float3 appendResult24_g180 = (float3(-break20_g180.z , 0.0 , break20_g180.x));
				float3 appendResult3_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g180 , 0.0 ));
				float3 lerpResult84_g180 = lerp( float3(0,1,0) , (transform4_g180).xyz , step( 1E-06 , ( abs( transform4_g180.x ) + abs( transform4_g180.z ) ) ));
				float3 normalizeResult7_g180 = normalize( lerpResult84_g180 );
				float dotResult9_g180 = dot( normalizeResult7_g180 , float3(0,1,0) );
				float temp_output_12_0_g180 = acos( dotResult9_g180 );
				float NaNPrevention21_g180 = step( 0.01 , abs( ( temp_output_12_0_g180 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g180 = lerp( float3(1,0,0) , appendResult24_g180 , NaNPrevention21_g180);
				float4 transform28_g180 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g180 , 0.0 ));
				float3 normalizeResult49_g180 = normalize( (transform28_g180).xyz );
				float3 RotationAxis30_g180 = normalizeResult49_g180;
				float SlopeCorrectionOffset191 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude189 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g180 = ( saturate( ( (0.0 + ((StaticWorldSpaceNoise148).x - 0.0) * (SlopeCorrectionOffset191 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude189 ) ) * temp_output_12_0_g180 );
				float3 rotatedValue35_g180 = RotateAroundAxis( Pivot113, ( v.vertex.xyz + MainBending89_g180 ), RotationAxis30_g180, RotationAngle29_g180 );
				float3 lerpResult52_g180 = lerp( MainBending89_g180 , ( rotatedValue35_g180 - v.vertex.xyz ) , NaNPrevention21_g180);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch203 = lerpResult52_g180;
				#else
				float3 staticSwitch203 = temp_output_190_0;
				#endif
				float3 LocalVertexOffset206 = staticSwitch203;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset206;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;
				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				float4 positionCS = TransformWorldToHClip( positionWS );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = positionCS;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				o.clipPos = positionCS;
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.ase_texcoord1 = v.ase_texcoord1;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.ase_texcoord1 = patch[0].ase_texcoord1 * bary.x + patch[1].ase_texcoord1 * bary.y + patch[2].ase_texcoord1 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex187 = IN.ase_texcoord2.xy;
				float4 tex2DNode187 = tex2D( _MainTex, uv_MainTex187 );
				float Opacity205 = tex2DNode187.a;
				
				float Alpha = Opacity205;
				float AlphaClipThreshold = _AlphaCutoff;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				#ifdef LOD_FADE_CROSSFADE
					LODDitheringTransition( IN.clipPos.xyz, unity_LODFade.x );
				#endif
				return 0;
			}
			ENDHLSL
		}

		
		Pass
		{
			
			Name "Meta"
			Tags { "LightMode"="Meta" }

			Cull Off

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _ALPHATEST_ON 1
			#define ASE_SRP_VERSION 70301

			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_META

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/MetaInput.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"

			#define ASE_NEEDS_VERT_POSITION
			#pragma shader_feature_local _ENABLESLOPECORRECTION_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct VertexOutput
			{
				float4 clipPos : SV_POSITION;
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 worldPos : TEXCOORD0;
				#endif
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
				float4 shadowCoord : TEXCOORD1;
				#endif
				float4 ase_texcoord2 : TEXCOORD2;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _Color1;
			float4 _NoiseTextureTilling;
			float4 _Color2;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _ColorBlendEnd;
			float _AlphaCutoff;
			#ifdef _TRANSMISSION_ASE
				float _TransmissionShadow;
			#endif
			#ifdef _TRANSLUCENCY_ASE
				float _TransStrength;
				float _TransNormal;
				float _TransScattering;
				float _TransDirect;
				float _TransAmbient;
				float _TransShadow;
			#endif
			#ifdef TESSELLATION_ON
				float _TessPhongStrength;
				float _TessValue;
				float _TessMin;
				float _TessMax;
				float _TessEdgeLength;
				float _TessMaxDisp;
			#endif
			CBUFFER_END
			float MBGlobalWindDir;
			sampler2D _NoiseTexture;
			sampler2D _MainTex;
			SAMPLER(sampler_MainTex);


			float3 RotateAroundAxis( float3 center, float3 original, float3 u, float angle )
			{
				original -= center;
				float C = cos( angle );
				float S = sin( angle );
				float t = 1 - C;
				float m00 = t * u.x * u.x + C;
				float m01 = t * u.x * u.y - S * u.z;
				float m02 = t * u.x * u.z + S * u.y;
				float m10 = t * u.x * u.y + S * u.z;
				float m11 = t * u.y * u.y + C;
				float m12 = t * u.y * u.z - S * u.x;
				float m20 = t * u.x * u.z - S * u.y;
				float m21 = t * u.y * u.z + S * u.x;
				float m22 = t * u.z * u.z + C;
				float3x3 finalMatrix = float3x3( m00, m01, m02, m10, m11, m12, m20, m21, m22 );
				return mul( finalMatrix, original ) + center;
			}
			

			VertexOutput VertexFunction( VertexInput v  )
			{
				VertexOutput o = (VertexOutput)0;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);

				float lerpResult133 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection149 = lerpResult133;
				float MB_WindDirectionOffset146 = _MBWindDirOffset;
				float4 transform1_g173 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord110 = v.texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult111 = (float3(texCoord110.x , 0.0 , texCoord110.y));
				float3 Pivot113 = -appendResult111;
				float4 transform2_g173 = mul(GetObjectToWorldMatrix(),float4( Pivot113 , 0.0 ));
				float2 UVs27_g174 = ( (transform1_g173).xz + (transform2_g173).xz );
				float4 temp_output_24_0_g174 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g174 = (temp_output_24_0_g174).zw;
				float2 panner7_g174 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldSpaceNoise147 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g174 * AnimatedNoiseTilling29_g174 ) + panner7_g174 ), 0, 0.0) );
				float temp_output_11_0_g178 = radians( ( ( MB_WindDirection149 + ( MB_WindDirectionOffset146 * (-1.0 + ((AnimatedWorldSpaceNoise147).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g178 = (float3(cos( temp_output_11_0_g178 ) , 0.0 , sin( temp_output_11_0_g178 )));
				float4 transform15_g178 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g178 , 0.0 ));
				float3 normalizeResult34_g178 = normalize( (transform15_g178).xyz );
				float3 MB_RotationAxis164 = normalizeResult34_g178;
				float MB_Amplitude145 = _MBAmplitude;
				float MB_AmplitudeOffset143 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g174 = (temp_output_24_0_g174).xy;
				float4 StaticWorldSpaceNoise148 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g174 * StaticNoileTilling28_g174 ), 0, 0.0) );
				float4 StaticWorldNoise31_g179 = StaticWorldSpaceNoise148;
				float4 transform8_g179 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency141 = _MBFrequency;
				float MB_FrequencyOffset138 = _MBFrequencyOffset;
				float DB_PhaseShift140 = v.ase_color.a;
				float MB_Phase144 = _MBPhase;
				float MB_DefaultBending137 = _MBDefaultBending;
				float MB_MaxHeight139 = _MBMaxHeight;
				float MB_RotationAngle166 = radians( ( ( ( ( MB_Amplitude145 + ( MB_AmplitudeOffset143 * (StaticWorldNoise31_g179).x ) ) * sin( ( ( ( transform8_g179.x + transform8_g179.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency141 + ( MB_FrequencyOffset138 * (StaticWorldNoise31_g179).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift140 ) ) ) * MB_Phase144 ) ) ) + MB_DefaultBending137 ) * ( v.vertex.xyz.y / MB_MaxHeight139 ) ) );
				float3 appendResult167 = (float3(v.vertex.xyz.x , 0.0 , v.vertex.xyz.z));
				float3 rotatedValue175 = RotateAroundAxis( appendResult167, v.vertex.xyz, MB_RotationAxis164, MB_RotationAngle166 );
				float3 temp_output_190_0 = ( ( rotatedValue175 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g180 = temp_output_190_0;
				float3 appendResult15_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g180 , 0.0 ));
				float4 break20_g180 = transform17_g180;
				float3 appendResult24_g180 = (float3(-break20_g180.z , 0.0 , break20_g180.x));
				float3 appendResult3_g180 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g180 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g180 , 0.0 ));
				float3 lerpResult84_g180 = lerp( float3(0,1,0) , (transform4_g180).xyz , step( 1E-06 , ( abs( transform4_g180.x ) + abs( transform4_g180.z ) ) ));
				float3 normalizeResult7_g180 = normalize( lerpResult84_g180 );
				float dotResult9_g180 = dot( normalizeResult7_g180 , float3(0,1,0) );
				float temp_output_12_0_g180 = acos( dotResult9_g180 );
				float NaNPrevention21_g180 = step( 0.01 , abs( ( temp_output_12_0_g180 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g180 = lerp( float3(1,0,0) , appendResult24_g180 , NaNPrevention21_g180);
				float4 transform28_g180 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g180 , 0.0 ));
				float3 normalizeResult49_g180 = normalize( (transform28_g180).xyz );
				float3 RotationAxis30_g180 = normalizeResult49_g180;
				float SlopeCorrectionOffset191 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude189 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g180 = ( saturate( ( (0.0 + ((StaticWorldSpaceNoise148).x - 0.0) * (SlopeCorrectionOffset191 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude189 ) ) * temp_output_12_0_g180 );
				float3 rotatedValue35_g180 = RotateAroundAxis( Pivot113, ( v.vertex.xyz + MainBending89_g180 ), RotationAxis30_g180, RotationAngle29_g180 );
				float3 lerpResult52_g180 = lerp( MainBending89_g180 , ( rotatedValue35_g180 - v.vertex.xyz ) , NaNPrevention21_g180);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch203 = lerpResult52_g180;
				#else
				float3 staticSwitch203 = temp_output_190_0;
				#endif
				float3 LocalVertexOffset206 = staticSwitch203;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset206;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					v.vertex.xyz = vertexValue;
				#else
					v.vertex.xyz += vertexValue;
				#endif

				v.ase_normal = v.ase_normal;

				float3 positionWS = TransformObjectToWorld( v.vertex.xyz );
				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				o.worldPos = positionWS;
				#endif

				o.clipPos = MetaVertexPosition( v.vertex, v.texcoord1.xy, v.texcoord1.xy, unity_LightmapST, unity_DynamicLightmapST );
				#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR) && defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					VertexPositionInputs vertexInput = (VertexPositionInputs)0;
					vertexInput.positionWS = positionWS;
					vertexInput.positionCS = o.clipPos;
					o.shadowCoord = GetShadowCoord( vertexInput );
				#endif
				return o;
			}

			#if defined(TESSELLATION_ON)
			struct VertexControl
			{
				float4 vertex : INTERNALTESSPOS;
				float3 ase_normal : NORMAL;
				float4 texcoord1 : TEXCOORD1;
				float4 texcoord2 : TEXCOORD2;
				float4 ase_color : COLOR;
				float4 ase_texcoord : TEXCOORD0;

				UNITY_VERTEX_INPUT_INSTANCE_ID
			};

			struct TessellationFactors
			{
				float edge[3] : SV_TessFactor;
				float inside : SV_InsideTessFactor;
			};

			VertexControl vert ( VertexInput v )
			{
				VertexControl o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_TRANSFER_INSTANCE_ID(v, o);
				o.vertex = v.vertex;
				o.ase_normal = v.ase_normal;
				o.texcoord1 = v.texcoord1;
				o.texcoord2 = v.texcoord2;
				o.ase_color = v.ase_color;
				o.ase_texcoord = v.ase_texcoord;
				return o;
			}

			TessellationFactors TessellationFunction (InputPatch<VertexControl,3> v)
			{
				TessellationFactors o;
				float4 tf = 1;
				float tessValue = _TessValue; float tessMin = _TessMin; float tessMax = _TessMax;
				float edgeLength = _TessEdgeLength; float tessMaxDisp = _TessMaxDisp;
				#if defined(ASE_FIXED_TESSELLATION)
				tf = FixedTess( tessValue );
				#elif defined(ASE_DISTANCE_TESSELLATION)
				tf = DistanceBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, tessValue, tessMin, tessMax, GetObjectToWorldMatrix(), _WorldSpaceCameraPos );
				#elif defined(ASE_LENGTH_TESSELLATION)
				tf = EdgeLengthBasedTess(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams );
				#elif defined(ASE_LENGTH_CULL_TESSELLATION)
				tf = EdgeLengthBasedTessCull(v[0].vertex, v[1].vertex, v[2].vertex, edgeLength, tessMaxDisp, GetObjectToWorldMatrix(), _WorldSpaceCameraPos, _ScreenParams, unity_CameraWorldClipPlanes );
				#endif
				o.edge[0] = tf.x; o.edge[1] = tf.y; o.edge[2] = tf.z; o.inside = tf.w;
				return o;
			}

			[domain("tri")]
			[partitioning("fractional_odd")]
			[outputtopology("triangle_cw")]
			[patchconstantfunc("TessellationFunction")]
			[outputcontrolpoints(3)]
			VertexControl HullFunction(InputPatch<VertexControl, 3> patch, uint id : SV_OutputControlPointID)
			{
			   return patch[id];
			}

			[domain("tri")]
			VertexOutput DomainFunction(TessellationFactors factors, OutputPatch<VertexControl, 3> patch, float3 bary : SV_DomainLocation)
			{
				VertexInput o = (VertexInput) 0;
				o.vertex = patch[0].vertex * bary.x + patch[1].vertex * bary.y + patch[2].vertex * bary.z;
				o.ase_normal = patch[0].ase_normal * bary.x + patch[1].ase_normal * bary.y + patch[2].ase_normal * bary.z;
				o.texcoord1 = patch[0].texcoord1 * bary.x + patch[1].texcoord1 * bary.y + patch[2].texcoord1 * bary.z;
				o.texcoord2 = patch[0].texcoord2 * bary.x + patch[1].texcoord2 * bary.y + patch[2].texcoord2 * bary.z;
				o.ase_color = patch[0].ase_color * bary.x + patch[1].ase_color * bary.y + patch[2].ase_color * bary.z;
				o.ase_texcoord = patch[0].ase_texcoord * bary.x + patch[1].ase_texcoord * bary.y + patch[2].ase_texcoord * bary.z;
				#if defined(ASE_PHONG_TESSELLATION)
				float3 pp[3];
				for (int i = 0; i < 3; ++i)
					pp[i] = o.vertex.xyz - patch[i].ase_normal * (dot(o.vertex.xyz, patch[i].ase_normal) - dot(patch[i].vertex.xyz, patch[i].ase_normal));
				float phongStrength = _TessPhongStrength;
				o.vertex.xyz = phongStrength * (pp[0]*bary.x + pp[1]*bary.y + pp[2]*bary.z) + (1.0f-phongStrength) * o.vertex.xyz;
				#endif
				UNITY_TRANSFER_INSTANCE_ID(patch[0], o);
				return VertexFunction(o);
			}
			#else
			VertexOutput vert ( VertexInput v )
			{
				return VertexFunction( v );
			}
			#endif

			half4 frag(VertexOutput IN  ) : SV_TARGET
			{
				UNITY_SETUP_INSTANCE_ID(IN);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX( IN );

				#if defined(ASE_NEEDS_FRAG_WORLD_POSITION)
				float3 WorldPosition = IN.worldPos;
				#endif
				float4 ShadowCoords = float4( 0, 0, 0, 0 );

				#if defined(ASE_NEEDS_FRAG_SHADOWCOORDS)
					#if defined(REQUIRES_VERTEX_SHADOW_COORD_INTERPOLATOR)
						ShadowCoords = IN.shadowCoord;
					#elif defined(MAIN_LIGHT_CALCULATE_SHADOWS)
						ShadowCoords = TransformWorldToShadowCoord( WorldPosition );
					#endif
				#endif

				float2 uv_MainTex187 = IN.ase_texcoord2.xy;
				float4 tex2DNode187 = tex2D( _MainTex, uv_MainTex187 );
				float MainTextureColor193 = tex2DNode187.r;
				float2 texCoord172 = IN.ase_texcoord2.xy * float2( 1,1 ) + float2( 0,0 );
				float smoothstepResult179 = smoothstep( _ColorBlendStart , _ColorBlendEnd , texCoord172.y);
				float4 lerpResult188 = lerp( _Color2 , _Color1 , smoothstepResult179);
				float4 Color194 = lerpResult188;
				float4 Albedo204 = ( MainTextureColor193 * Color194 );
				
				float Opacity205 = tex2DNode187.a;
				
				
				float3 Albedo = Albedo204.rgb;
				float3 Emission = 0;
				float Alpha = Opacity205;
				float AlphaClipThreshold = _AlphaCutoff;

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				MetaInput metaInput = (MetaInput)0;
				metaInput.Albedo = Albedo;
				metaInput.Emission = Emission;
				
				return MetaFragment(metaInput);
			}
			ENDHLSL
		}

	
	}
	/*ase_lod*/
	CustomEditor "StylisedGrass_MaterialInspector"
	
	
}
/*ASEBEGIN
Version=18600
2352.286;756.1429;1083;565;3157.08;-1072.017;1;True;False
Node;AmplifyShaderEditor.CommentaryNode;109;-3197.466,897.6841;Inherit;False;885.0664;507.0813;;6;140;136;113;111;110;208;Vertex Colors and UVs Baked Data;1,1,1,1;0;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;110;-3155.684,1214.937;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;111;-2889.355,1223.401;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NegateNode;208;-2707.08,1223.017;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;113;-2532.355,1219.401;Inherit;False;Pivot;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;117;-3837.302,-123.624;Inherit;False;1404.776;763.0399;;8;114;148;115;147;126;124;123;121;World Position Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;116;-4989.456,-638.8433;Inherit;False;894.3172;1282.288;;25;183;181;128;131;122;119;120;129;132;127;130;134;135;191;189;149;146;145;144;143;141;139;138;137;133;Material Properties;1,1,1,1;0;0
Node;AmplifyShaderEditor.GetLocalVarNode;114;-3799.2,-12.69401;Inherit;False;113;Pivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;119;-4889.957,88.4707;Inherit;False;Global;MBGlobalWindDir;MB Global Wind Dir;28;1;[HideInInspector];Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;121;-3657.725,273.7649;Inherit;False;Property;_NoiseTextureTilling;Noise Tilling - Static (XY), Animated (ZW);20;0;Create;False;0;0;False;0;False;1,1,1,1;1,1,1,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector2Node;123;-3583.172,450.5669;Float;False;Property;_NoisePannerSpeed;Noise Panner Speed;21;0;Create;True;0;0;False;0;False;0.05,0.03;0.08,0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.FunctionNode;115;-3625.787,-8.780012;Inherit;False;WorldSpaceUVs - NHP;-1;;173;88a2e8a391a04e241878bdb87d9283a3;0;1;6;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;122;-4887.597,171.2928;Inherit;False;Property;_MBWindDirBlend;MB Wind Dir Blend;13;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;120;-4889.201,8.102621;Float;False;Property;_MBWindDir;MB Wind Dir;11;0;Create;False;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;124;-3619.461,70.58083;Inherit;True;Property;_NoiseTexture;Noise Texture;19;1;[NoScaleOffset];Create;True;0;0;False;0;False;512fa11ad89d84543ad8d6c8d9cb6743;512fa11ad89d84543ad8d6c8d9cb6743;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RangedFloatNode;128;-4889.389,362.7909;Inherit;False;Property;_MBMaxHeight;MB Max Height;15;0;Create;True;0;0;False;0;False;1;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;127;-4889.557,-272.7902;Float;False;Property;_MBFrequency;MB Frequency;9;0;Create;True;0;0;False;0;False;1.11;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;136;-3160.672,960.2532;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;132;-4887.458,-178.6562;Inherit;False;Property;_MBFrequencyOffset;MB Frequency Offset;10;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;135;-4889.557,-560.7894;Float;False;Property;_MBDefaultBending;MB Default Bending;6;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;131;-4886.679,275.5277;Float;False;Property;_MBWindDirOffset;MB Wind Dir Offset;12;0;Create;True;0;0;False;0;False;20;0;0;180;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;126;-3255.315,193.7058;Inherit;False;WorldSpaceNoise - NHP;-1;;174;af5fa9ff24e18344ebcc05b64d296c57;0;4;22;FLOAT2;0,0;False;20;SAMPLER2D;;False;24;FLOAT4;1,1,1,1;False;19;FLOAT2;0.1,0.1;False;2;COLOR;0;COLOR;16
Node;AmplifyShaderEditor.LerpOp;133;-4570.892,70.62454;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;129;-4886.159,-81.92039;Float;False;Property;_MBPhase;MB Phase;14;0;Create;True;0;0;False;0;False;3;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;134;-4889.557,-464.7902;Float;False;Property;_MBAmplitude;MB Amplitude;7;0;Create;True;0;0;False;0;False;1.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;130;-4889.557,-368.7902;Float;False;Property;_MBAmplitudeOffset;MB Amplitude Offset;8;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;144;-4368.159,-81.92039;Inherit;False;MB_Phase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;143;-4403.557,-368.7902;Inherit;False;MB_AmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;148;-2682.89,149.3622;Inherit;False;StaticWorldSpaceNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;139;-4369.333,359.7899;Inherit;False;MB_MaxHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;138;-4405.561,-178.4983;Inherit;False;MB_FrequencyOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;140;-2708.056,1048.136;Float;False;DB_PhaseShift;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;147;-2696.9,259.291;Inherit;False;AnimatedWorldSpaceNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;142;-2172.327,-637.3311;Inherit;False;3578.668;1276.024;;40;206;203;199;192;198;196;197;2;4;5;3;0;190;185;182;177;176;178;175;168;167;169;171;164;166;165;162;163;158;151;160;153;154;150;156;159;157;152;155;161;Main Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;137;-4403.557,-560.7894;Float;False;MB_DefaultBending;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;145;-4371.557,-464.7902;Float;False;MB_Amplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;146;-4430.389,273.3368;Inherit;False;MB_WindDirectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;141;-4371.557,-272.7902;Float;False;MB_Frequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;149;-4388.159,64.52962;Float;False;MB_WindDirection;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;158;-2054.817,518.8262;Inherit;False;148;StaticWorldSpaceNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;151;-2006.828,46.83008;Inherit;False;141;MB_Frequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;160;-2039.152,-245.1699;Inherit;False;137;MB_DefaultBending;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;153;-2007.756,424.0032;Inherit;False;139;MB_MaxHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;154;-2042.756,140.2991;Inherit;False;138;MB_FrequencyOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;155;-2043.828,-50.16992;Inherit;False;143;MB_AmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;152;-2030.094,-571.8831;Inherit;False;149;MB_WindDirection;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;161;-2064.764,-467.7639;Inherit;False;146;MB_WindDirectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;156;-2081.621,-374.844;Inherit;False;147;AnimatedWorldSpaceNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;159;-1981.828,241.8301;Inherit;False;144;MB_Phase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;157;-2004.828,-149.1699;Inherit;False;145;MB_Amplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;150;-2008.293,332.4512;Inherit;False;140;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;162;-1684.518,54.09302;Inherit;False;RotationAngle - NHP;-1;;179;87b0b7c0fc8f1424db43b84d20c2e79b;0;9;36;FLOAT;0;False;35;FLOAT;0;False;34;FLOAT;1;False;28;FLOAT;1;False;47;FLOAT;0;False;29;FLOAT;1;False;46;FLOAT;0;False;42;FLOAT;0.1;False;27;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;163;-1704.405,-492.729;Inherit;False;RotationAxis - NHP;-1;;178;b90648f17dcc4bc449d46e8cf04564ff;0;3;20;FLOAT;0;False;19;FLOAT;0;False;18;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;165;-953.2468,-242.6079;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;170;-125.4079,-1790.479;Inherit;False;1528.726;892.1869;;15;205;204;202;201;200;193;194;188;187;179;184;180;173;172;174;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;166;-1242.234,48.21899;Float;False;MB_RotationAngle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;164;-1341.835,-497.2429;Inherit;False;MB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;168;-769.3083,-88.60791;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;173;-55.20882,-1323.726;Inherit;False;Property;_ColorBlendStart;Color Blend Start;3;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;169;-792.157,-413.8291;Inherit;False;164;MB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;172;-14.71082,-1461.846;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;171;-802.8445,-317.2041;Inherit;False;166;MB_RotationAngle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;167;-720.2458,-219.6079;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;174;-54.93893,-1225.673;Inherit;False;Property;_ColorBlendEnd;Color Blend End;4;0;Create;True;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.PosVertexDataNode;178;-396.7328,-124.665;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RotateAboutAxisNode;175;-505.7699,-280.9128;Inherit;False;False;4;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.PosVertexDataNode;177;-392.0459,134.554;Inherit;False;0;0;5;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;180;246.9834,-1705.974;Inherit;False;Property;_Color2;Color 2;2;0;Create;True;0;0;False;0;False;0.719914,0.8207547,0.3639195,1;0.009433985,0.4014694,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;176;-364.0908,48.64917;Float;False;Constant;_Float11;Float 11;8;0;Create;True;0;0;False;0;False;0.01;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;181;-4891.542,456.3909;Inherit;False;Property;_SlopeCorrectionMagnitude;Slope Correction Magnitude;17;0;Create;True;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;184;251.1824,-1525.19;Inherit;False;Property;_Color1;Color 1;1;0;Create;True;0;0;False;0;False;0.3393185,0.490566,0.09255961,1;0.5754717,0.3435538,0.1465824,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SmoothstepOpNode;179;304.0614,-1342.673;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;183;-4890.394,544.9398;Inherit;False;Property;_SlopeCorrectionOffset;Slope Correction Offset;18;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;191;-4419.394,544.9398;Inherit;False;SlopeCorrectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;189;-4446.542,455.3909;Inherit;False;SlopeCorrectionMagnitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;185;-143.0648,-207.856;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;188;526.1661,-1540.991;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SamplerNode;187;-85.19571,-1099.25;Inherit;True;Property;_MainTex;Main Tex;0;1;[NoScaleOffset];Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StepOpNode;182;-127.9159,99.99707;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;190;81.16418,-53.50586;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;198;53.79421,269.9858;Inherit;False;148;StaticWorldSpaceNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;197;29.79419,102.9854;Inherit;False;189;SlopeCorrectionMagnitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;193;265.3263,-1097.768;Inherit;False;MainTextureColor;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;196;126.7943,349.9858;Inherit;False;113;Pivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;194;725.3461,-1547.158;Inherit;False;Color;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;192;59.79421,187.9853;Inherit;False;191;SlopeCorrectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;200;673.5396,-1277.647;Inherit;False;193;MainTextureColor;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;199;395.7946,139.9853;Inherit;False;SlopeCorrection - NHP;-1;;180;af38de3ca0adf3c4ba9b6a3dd482959e;0;5;87;FLOAT3;0,0,0;False;42;FLOAT;1;False;92;FLOAT;0;False;93;FLOAT4;0,0,0,0;False;41;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;201;721.7816,-1180.122;Inherit;False;194;Color;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;202;940.1597,-1236.456;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;203;886.4929,-63.51478;Float;False;Property;_EnableSlopeCorrection;Enable Slope Correction;16;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;1102.76,-1242.31;Float;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;206;1194.493,-64.51479;Float;False;LocalVertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;102;1801.3,-1090.482;Inherit;False;763.4954;643.0168;;6;1;105;207;108;106;103;Master Node;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;205;269.3044,-998.7187;Float;False;Opacity;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;105;1906.157,-590.9363;Inherit;False;206;LocalVertexOffset;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;103;1959.81,-1009.516;Inherit;False;204;Albedo;1;0;OBJECT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;106;1955.348,-807.6854;Inherit;False;205;Opacity;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;207;1851.817,-710.533;Inherit;False;Property;_AlphaCutoff;Alpha Cutoff;5;0;Create;True;0;0;False;0;False;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;108;1987.263,-897.8419;Inherit;False;Constant;_Float2;Float 2;22;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;-33,-39;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;False;False;False;False;0;False;-1;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;2;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;2203.058,-906.812;Float;False;True;-1;2;StylisedGrass_MaterialInspector;0;2;Nicrom/NHP/ASE/Stylised Grass;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;17;False;False;False;False;False;False;False;False;True;1;False;-1;True;2;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;2;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;;0;0;Standard;36;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;0;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;1;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;False;False;;False;0
WireConnection;111;0;110;1
WireConnection;111;2;110;2
WireConnection;208;0;111;0
WireConnection;113;0;208;0
WireConnection;115;6;114;0
WireConnection;126;22;115;0
WireConnection;126;20;124;0
WireConnection;126;24;121;0
WireConnection;126;19;123;0
WireConnection;133;0;120;0
WireConnection;133;1;119;0
WireConnection;133;2;122;0
WireConnection;144;0;129;0
WireConnection;143;0;130;0
WireConnection;148;0;126;0
WireConnection;139;0;128;0
WireConnection;138;0;132;0
WireConnection;140;0;136;4
WireConnection;147;0;126;16
WireConnection;137;0;135;0
WireConnection;145;0;134;0
WireConnection;146;0;131;0
WireConnection;141;0;127;0
WireConnection;149;0;133;0
WireConnection;162;36;160;0
WireConnection;162;35;157;0
WireConnection;162;34;155;0
WireConnection;162;28;151;0
WireConnection;162;47;154;0
WireConnection;162;29;159;0
WireConnection;162;46;150;0
WireConnection;162;42;153;0
WireConnection;162;27;158;0
WireConnection;163;20;152;0
WireConnection;163;19;161;0
WireConnection;163;18;156;0
WireConnection;166;0;162;0
WireConnection;164;0;163;0
WireConnection;167;0;165;1
WireConnection;167;2;165;3
WireConnection;175;0;169;0
WireConnection;175;1;171;0
WireConnection;175;2;167;0
WireConnection;175;3;168;0
WireConnection;179;0;172;2
WireConnection;179;1;173;0
WireConnection;179;2;174;0
WireConnection;191;0;183;0
WireConnection;189;0;181;0
WireConnection;185;0;175;0
WireConnection;185;1;178;0
WireConnection;188;0;180;0
WireConnection;188;1;184;0
WireConnection;188;2;179;0
WireConnection;182;0;176;0
WireConnection;182;1;177;2
WireConnection;190;0;185;0
WireConnection;190;1;182;0
WireConnection;193;0;187;1
WireConnection;194;0;188;0
WireConnection;199;87;190;0
WireConnection;199;42;197;0
WireConnection;199;92;192;0
WireConnection;199;93;198;0
WireConnection;199;41;196;0
WireConnection;202;0;200;0
WireConnection;202;1;201;0
WireConnection;203;1;190;0
WireConnection;203;0;199;0
WireConnection;204;0;202;0
WireConnection;206;0;203;0
WireConnection;205;0;187;4
WireConnection;1;0;103;0
WireConnection;1;3;108;0
WireConnection;1;4;108;0
WireConnection;1;6;106;0
WireConnection;1;7;207;0
WireConnection;1;8;105;0
ASEEND*/
//CHKSM=2C580429691D27110406368BC13CD3D7657B78B0