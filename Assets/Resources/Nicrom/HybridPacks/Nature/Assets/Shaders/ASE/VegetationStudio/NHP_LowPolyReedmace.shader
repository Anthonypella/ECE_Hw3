// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Nicrom/NHP/ASE//Vegetation Studio/Low Poly Reedmace"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_Metallic("Metallic", Range( 0 , 1)) = 0
		_Smoothness("Smoothness", Range( 0 , 1)) = 0
		[NoScaleOffset][Header(Textures)]_MainTex("Main Texture", 2D) = "white" {}
		[NoScaleOffset]_NoiseTexture("Noise Texture", 2D) = "white" {}
		_NoiseTextureTilling("Noise Tilling - Static (XY), Animated (ZW)", Vector) = (1,1,1,1)
		_NoisePannerSpeed("Noise Panner Speed", Vector) = (0.05,0.03,0,0)
		_MBDefaultBending("MB Default Bending", Float) = 0
		_MBAmplitude("MB Amplitude", Float) = 1.5
		_MBAmplitudeOffset("MB Amplitude Offset", Float) = 2
		_MBFrequency("MB Frequency", Float) = 1.11
		_MBFrequencyOffset("MB Frequency Offset", Float) = 0
		_MBPhase("MB Phase", Float) = 1
		_MBWindDir("MB Wind Dir", Range( 0 , 360)) = 0
		_MBWindDirOffset("MB Wind Dir Offset", Range( 0 , 180)) = 20
		_MBWindDirBlend("MB Wind Dir Blend", Range( 0 , 1)) = 0
		_MBMaxHeight("MB Max Height", Float) = 10
		[Toggle(_ENABLESLOPECORRECTION_ON)] _EnableSlopeCorrection("Enable Slope Correction", Float) = 1
		_SlopeCorrectionMagnitude("Slope Correction Magnitude", Range( 0 , 1)) = 1
		[ASEEnd]_SlopeCorrectionOffset("Slope Correction Offset", Range( 0 , 1)) = 0

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
		Cull Back
		AlphaToMask Off
		HLSLINCLUDE
		#pragma target 2.0

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
			#pragma multi_compile_fog
			#define ASE_FOG 1
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
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setup
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
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
			float _Metallic;
			float _Smoothness;
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

				float lerpResult99 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection119 = lerpResult99;
				float MB_WindDirectionVariation110 = _MBWindDirOffset;
				float4 transform1_g122 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord125 = v.texcoord1.xyzw.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult136 = (float3(texCoord125.x , 0.0 , texCoord125.y));
				float3 MB_LocalPivot139 = -appendResult136;
				float4 transform2_g122 = mul(GetObjectToWorldMatrix(),float4( MB_LocalPivot139 , 0.0 ));
				float2 UVs27_g115 = ( (transform1_g122).xz + (transform2_g122).xz );
				float4 temp_output_24_0_g115 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g115 = (temp_output_24_0_g115).zw;
				float2 panner7_g115 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise121 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g115 * AnimatedNoiseTilling29_g115 ) + panner7_g115 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection119 + ( MB_WindDirectionVariation110 * (-1.0 + ((AnimatedNoise121).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis138 = normalizeResult34_g118;
				float3 RotationAxis56_g120 = MB_RotationAxis138;
				float MB_Amplitude109 = _MBAmplitude;
				float MB_AmplitudeOffset115 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g115 = (temp_output_24_0_g115).xy;
				float4 StaticNoise120 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g115 * StaticNoileTilling28_g115 ), 0, 0.0) );
				float4 StaticWorldNoise31_g119 = StaticNoise120;
				float4 transform8_g119 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency118 = _MBFrequency;
				float MB_FrequencyOffset113 = _MBFrequencyOffset;
				float PhaseShift112 = v.ase_color.a;
				float MB_Phase117 = _MBPhase;
				float MB_DefaultBending111 = _MBDefaultBending;
				float MB_MaxHeight114 = _MBMaxHeight;
				float MB_RotationAngle140 = radians( ( ( ( ( MB_Amplitude109 + ( MB_AmplitudeOffset115 * (StaticWorldNoise31_g119).x ) ) * sin( ( ( ( transform8_g119.x + transform8_g119.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency118 + ( MB_FrequencyOffset113 * (StaticWorldNoise31_g119).x ) ) ) + ( ( 2.0 * PI ) * PhaseShift112 ) ) ) * MB_Phase117 ) ) ) + MB_DefaultBending111 ) * ( v.vertex.xyz.y / MB_MaxHeight114 ) ) );
				float RotationAngle54_g120 = MB_RotationAngle140;
				float3 PivotPoint60_g120 = MB_LocalPivot139;
				float3 break62_g120 = PivotPoint60_g120;
				float3 appendResult45_g120 = (float3(break62_g120.x , v.vertex.xyz.y , break62_g120.z));
				float3 rotatedValue30_g120 = RotateAroundAxis( appendResult45_g120, v.vertex.xyz, RotationAxis56_g120, RotationAngle54_g120 );
				float3 rotatedValue34_g120 = RotateAroundAxis( PivotPoint60_g120, ( rotatedValue30_g120 + float3( 0,0,0 ) ), RotationAxis56_g120, RotationAngle54_g120 );
				float3 temp_output_149_0 = ( ( rotatedValue34_g120 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g121 = temp_output_149_0;
				float3 appendResult15_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g121 , 0.0 ));
				float4 break20_g121 = transform17_g121;
				float3 appendResult24_g121 = (float3(-break20_g121.z , 0.0 , break20_g121.x));
				float3 appendResult3_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g121 , 0.0 ));
				float3 lerpResult84_g121 = lerp( float3(0,1,0) , (transform4_g121).xyz , step( 1E-06 , ( abs( transform4_g121.x ) + abs( transform4_g121.z ) ) ));
				float3 normalizeResult7_g121 = normalize( lerpResult84_g121 );
				float dotResult9_g121 = dot( normalizeResult7_g121 , float3(0,1,0) );
				float temp_output_12_0_g121 = acos( dotResult9_g121 );
				float NaNPrevention21_g121 = step( 0.01 , abs( ( temp_output_12_0_g121 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g121 = lerp( float3(1,0,0) , appendResult24_g121 , NaNPrevention21_g121);
				float4 transform28_g121 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g121 , 0.0 ));
				float3 normalizeResult49_g121 = normalize( (transform28_g121).xyz );
				float3 RotationAxis30_g121 = normalizeResult49_g121;
				float SlopeCorrectionOffset150 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude146 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g121 = ( saturate( ( (0.0 + ((StaticNoise120).x - 0.0) * (SlopeCorrectionOffset150 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude146 ) ) * temp_output_12_0_g121 );
				float3 rotatedValue35_g121 = RotateAroundAxis( MB_LocalPivot139, ( v.vertex.xyz + MainBending89_g121 ), RotationAxis30_g121, RotationAngle29_g121 );
				float3 lerpResult52_g121 = lerp( MainBending89_g121 , ( rotatedValue35_g121 - v.vertex.xyz ) , NaNPrevention21_g121);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch159 = lerpResult52_g121;
				#else
				float3 staticSwitch159 = temp_output_149_0;
				#endif
				float3 LocalVertexOffset163 = staticSwitch159;
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset163;
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

				float2 uv_MainTex = IN.ase_texcoord7.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float4 Albedo162 = tex2D( _MainTex, uv_MainTex );
				
				float3 Albedo = Albedo162.rgb;
				float3 Normal = float3(0, 0, 1);
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = _Metallic;
				float Smoothness = _Smoothness;
				float Occlusion = 1;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
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
			#pragma multi_compile_fog
			#define ASE_FOG 1
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
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setup
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;
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
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
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
			float _Metallic;
			float _Smoothness;
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

				float lerpResult99 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection119 = lerpResult99;
				float MB_WindDirectionVariation110 = _MBWindDirOffset;
				float4 transform1_g122 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord125 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult136 = (float3(texCoord125.x , 0.0 , texCoord125.y));
				float3 MB_LocalPivot139 = -appendResult136;
				float4 transform2_g122 = mul(GetObjectToWorldMatrix(),float4( MB_LocalPivot139 , 0.0 ));
				float2 UVs27_g115 = ( (transform1_g122).xz + (transform2_g122).xz );
				float4 temp_output_24_0_g115 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g115 = (temp_output_24_0_g115).zw;
				float2 panner7_g115 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise121 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g115 * AnimatedNoiseTilling29_g115 ) + panner7_g115 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection119 + ( MB_WindDirectionVariation110 * (-1.0 + ((AnimatedNoise121).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis138 = normalizeResult34_g118;
				float3 RotationAxis56_g120 = MB_RotationAxis138;
				float MB_Amplitude109 = _MBAmplitude;
				float MB_AmplitudeOffset115 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g115 = (temp_output_24_0_g115).xy;
				float4 StaticNoise120 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g115 * StaticNoileTilling28_g115 ), 0, 0.0) );
				float4 StaticWorldNoise31_g119 = StaticNoise120;
				float4 transform8_g119 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency118 = _MBFrequency;
				float MB_FrequencyOffset113 = _MBFrequencyOffset;
				float PhaseShift112 = v.ase_color.a;
				float MB_Phase117 = _MBPhase;
				float MB_DefaultBending111 = _MBDefaultBending;
				float MB_MaxHeight114 = _MBMaxHeight;
				float MB_RotationAngle140 = radians( ( ( ( ( MB_Amplitude109 + ( MB_AmplitudeOffset115 * (StaticWorldNoise31_g119).x ) ) * sin( ( ( ( transform8_g119.x + transform8_g119.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency118 + ( MB_FrequencyOffset113 * (StaticWorldNoise31_g119).x ) ) ) + ( ( 2.0 * PI ) * PhaseShift112 ) ) ) * MB_Phase117 ) ) ) + MB_DefaultBending111 ) * ( v.vertex.xyz.y / MB_MaxHeight114 ) ) );
				float RotationAngle54_g120 = MB_RotationAngle140;
				float3 PivotPoint60_g120 = MB_LocalPivot139;
				float3 break62_g120 = PivotPoint60_g120;
				float3 appendResult45_g120 = (float3(break62_g120.x , v.vertex.xyz.y , break62_g120.z));
				float3 rotatedValue30_g120 = RotateAroundAxis( appendResult45_g120, v.vertex.xyz, RotationAxis56_g120, RotationAngle54_g120 );
				float3 rotatedValue34_g120 = RotateAroundAxis( PivotPoint60_g120, ( rotatedValue30_g120 + float3( 0,0,0 ) ), RotationAxis56_g120, RotationAngle54_g120 );
				float3 temp_output_149_0 = ( ( rotatedValue34_g120 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g121 = temp_output_149_0;
				float3 appendResult15_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g121 , 0.0 ));
				float4 break20_g121 = transform17_g121;
				float3 appendResult24_g121 = (float3(-break20_g121.z , 0.0 , break20_g121.x));
				float3 appendResult3_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g121 , 0.0 ));
				float3 lerpResult84_g121 = lerp( float3(0,1,0) , (transform4_g121).xyz , step( 1E-06 , ( abs( transform4_g121.x ) + abs( transform4_g121.z ) ) ));
				float3 normalizeResult7_g121 = normalize( lerpResult84_g121 );
				float dotResult9_g121 = dot( normalizeResult7_g121 , float3(0,1,0) );
				float temp_output_12_0_g121 = acos( dotResult9_g121 );
				float NaNPrevention21_g121 = step( 0.01 , abs( ( temp_output_12_0_g121 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g121 = lerp( float3(1,0,0) , appendResult24_g121 , NaNPrevention21_g121);
				float4 transform28_g121 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g121 , 0.0 ));
				float3 normalizeResult49_g121 = normalize( (transform28_g121).xyz );
				float3 RotationAxis30_g121 = normalizeResult49_g121;
				float SlopeCorrectionOffset150 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude146 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g121 = ( saturate( ( (0.0 + ((StaticNoise120).x - 0.0) * (SlopeCorrectionOffset150 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude146 ) ) * temp_output_12_0_g121 );
				float3 rotatedValue35_g121 = RotateAroundAxis( MB_LocalPivot139, ( v.vertex.xyz + MainBending89_g121 ), RotationAxis30_g121, RotationAngle29_g121 );
				float3 lerpResult52_g121 = lerp( MainBending89_g121 , ( rotatedValue35_g121 - v.vertex.xyz ) , NaNPrevention21_g121);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch159 = lerpResult52_g121;
				#else
				float3 staticSwitch159 = temp_output_149_0;
				#endif
				float3 LocalVertexOffset163 = staticSwitch159;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset163;
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

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;
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
			#pragma multi_compile_fog
			#define ASE_FOG 1
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
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setup
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_color : COLOR;
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
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
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
			float _Metallic;
			float _Smoothness;
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

				float lerpResult99 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection119 = lerpResult99;
				float MB_WindDirectionVariation110 = _MBWindDirOffset;
				float4 transform1_g122 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord125 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult136 = (float3(texCoord125.x , 0.0 , texCoord125.y));
				float3 MB_LocalPivot139 = -appendResult136;
				float4 transform2_g122 = mul(GetObjectToWorldMatrix(),float4( MB_LocalPivot139 , 0.0 ));
				float2 UVs27_g115 = ( (transform1_g122).xz + (transform2_g122).xz );
				float4 temp_output_24_0_g115 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g115 = (temp_output_24_0_g115).zw;
				float2 panner7_g115 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise121 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g115 * AnimatedNoiseTilling29_g115 ) + panner7_g115 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection119 + ( MB_WindDirectionVariation110 * (-1.0 + ((AnimatedNoise121).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis138 = normalizeResult34_g118;
				float3 RotationAxis56_g120 = MB_RotationAxis138;
				float MB_Amplitude109 = _MBAmplitude;
				float MB_AmplitudeOffset115 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g115 = (temp_output_24_0_g115).xy;
				float4 StaticNoise120 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g115 * StaticNoileTilling28_g115 ), 0, 0.0) );
				float4 StaticWorldNoise31_g119 = StaticNoise120;
				float4 transform8_g119 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency118 = _MBFrequency;
				float MB_FrequencyOffset113 = _MBFrequencyOffset;
				float PhaseShift112 = v.ase_color.a;
				float MB_Phase117 = _MBPhase;
				float MB_DefaultBending111 = _MBDefaultBending;
				float MB_MaxHeight114 = _MBMaxHeight;
				float MB_RotationAngle140 = radians( ( ( ( ( MB_Amplitude109 + ( MB_AmplitudeOffset115 * (StaticWorldNoise31_g119).x ) ) * sin( ( ( ( transform8_g119.x + transform8_g119.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency118 + ( MB_FrequencyOffset113 * (StaticWorldNoise31_g119).x ) ) ) + ( ( 2.0 * PI ) * PhaseShift112 ) ) ) * MB_Phase117 ) ) ) + MB_DefaultBending111 ) * ( v.vertex.xyz.y / MB_MaxHeight114 ) ) );
				float RotationAngle54_g120 = MB_RotationAngle140;
				float3 PivotPoint60_g120 = MB_LocalPivot139;
				float3 break62_g120 = PivotPoint60_g120;
				float3 appendResult45_g120 = (float3(break62_g120.x , v.vertex.xyz.y , break62_g120.z));
				float3 rotatedValue30_g120 = RotateAroundAxis( appendResult45_g120, v.vertex.xyz, RotationAxis56_g120, RotationAngle54_g120 );
				float3 rotatedValue34_g120 = RotateAroundAxis( PivotPoint60_g120, ( rotatedValue30_g120 + float3( 0,0,0 ) ), RotationAxis56_g120, RotationAngle54_g120 );
				float3 temp_output_149_0 = ( ( rotatedValue34_g120 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g121 = temp_output_149_0;
				float3 appendResult15_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g121 , 0.0 ));
				float4 break20_g121 = transform17_g121;
				float3 appendResult24_g121 = (float3(-break20_g121.z , 0.0 , break20_g121.x));
				float3 appendResult3_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g121 , 0.0 ));
				float3 lerpResult84_g121 = lerp( float3(0,1,0) , (transform4_g121).xyz , step( 1E-06 , ( abs( transform4_g121.x ) + abs( transform4_g121.z ) ) ));
				float3 normalizeResult7_g121 = normalize( lerpResult84_g121 );
				float dotResult9_g121 = dot( normalizeResult7_g121 , float3(0,1,0) );
				float temp_output_12_0_g121 = acos( dotResult9_g121 );
				float NaNPrevention21_g121 = step( 0.01 , abs( ( temp_output_12_0_g121 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g121 = lerp( float3(1,0,0) , appendResult24_g121 , NaNPrevention21_g121);
				float4 transform28_g121 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g121 , 0.0 ));
				float3 normalizeResult49_g121 = normalize( (transform28_g121).xyz );
				float3 RotationAxis30_g121 = normalizeResult49_g121;
				float SlopeCorrectionOffset150 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude146 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g121 = ( saturate( ( (0.0 + ((StaticNoise120).x - 0.0) * (SlopeCorrectionOffset150 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude146 ) ) * temp_output_12_0_g121 );
				float3 rotatedValue35_g121 = RotateAroundAxis( MB_LocalPivot139, ( v.vertex.xyz + MainBending89_g121 ), RotationAxis30_g121, RotationAngle29_g121 );
				float3 lerpResult52_g121 = lerp( MainBending89_g121 , ( rotatedValue35_g121 - v.vertex.xyz ) , NaNPrevention21_g121);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch159 = lerpResult52_g121;
				#else
				float3 staticSwitch159 = temp_output_149_0;
				#endif
				float3 LocalVertexOffset163 = staticSwitch159;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset163;
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

				
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

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
			#pragma multi_compile_fog
			#define ASE_FOG 1
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
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setup
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
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
			float _Metallic;
			float _Smoothness;
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

				float lerpResult99 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection119 = lerpResult99;
				float MB_WindDirectionVariation110 = _MBWindDirOffset;
				float4 transform1_g122 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord125 = v.texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult136 = (float3(texCoord125.x , 0.0 , texCoord125.y));
				float3 MB_LocalPivot139 = -appendResult136;
				float4 transform2_g122 = mul(GetObjectToWorldMatrix(),float4( MB_LocalPivot139 , 0.0 ));
				float2 UVs27_g115 = ( (transform1_g122).xz + (transform2_g122).xz );
				float4 temp_output_24_0_g115 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g115 = (temp_output_24_0_g115).zw;
				float2 panner7_g115 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise121 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g115 * AnimatedNoiseTilling29_g115 ) + panner7_g115 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection119 + ( MB_WindDirectionVariation110 * (-1.0 + ((AnimatedNoise121).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis138 = normalizeResult34_g118;
				float3 RotationAxis56_g120 = MB_RotationAxis138;
				float MB_Amplitude109 = _MBAmplitude;
				float MB_AmplitudeOffset115 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g115 = (temp_output_24_0_g115).xy;
				float4 StaticNoise120 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g115 * StaticNoileTilling28_g115 ), 0, 0.0) );
				float4 StaticWorldNoise31_g119 = StaticNoise120;
				float4 transform8_g119 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency118 = _MBFrequency;
				float MB_FrequencyOffset113 = _MBFrequencyOffset;
				float PhaseShift112 = v.ase_color.a;
				float MB_Phase117 = _MBPhase;
				float MB_DefaultBending111 = _MBDefaultBending;
				float MB_MaxHeight114 = _MBMaxHeight;
				float MB_RotationAngle140 = radians( ( ( ( ( MB_Amplitude109 + ( MB_AmplitudeOffset115 * (StaticWorldNoise31_g119).x ) ) * sin( ( ( ( transform8_g119.x + transform8_g119.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency118 + ( MB_FrequencyOffset113 * (StaticWorldNoise31_g119).x ) ) ) + ( ( 2.0 * PI ) * PhaseShift112 ) ) ) * MB_Phase117 ) ) ) + MB_DefaultBending111 ) * ( v.vertex.xyz.y / MB_MaxHeight114 ) ) );
				float RotationAngle54_g120 = MB_RotationAngle140;
				float3 PivotPoint60_g120 = MB_LocalPivot139;
				float3 break62_g120 = PivotPoint60_g120;
				float3 appendResult45_g120 = (float3(break62_g120.x , v.vertex.xyz.y , break62_g120.z));
				float3 rotatedValue30_g120 = RotateAroundAxis( appendResult45_g120, v.vertex.xyz, RotationAxis56_g120, RotationAngle54_g120 );
				float3 rotatedValue34_g120 = RotateAroundAxis( PivotPoint60_g120, ( rotatedValue30_g120 + float3( 0,0,0 ) ), RotationAxis56_g120, RotationAngle54_g120 );
				float3 temp_output_149_0 = ( ( rotatedValue34_g120 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g121 = temp_output_149_0;
				float3 appendResult15_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g121 , 0.0 ));
				float4 break20_g121 = transform17_g121;
				float3 appendResult24_g121 = (float3(-break20_g121.z , 0.0 , break20_g121.x));
				float3 appendResult3_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g121 , 0.0 ));
				float3 lerpResult84_g121 = lerp( float3(0,1,0) , (transform4_g121).xyz , step( 1E-06 , ( abs( transform4_g121.x ) + abs( transform4_g121.z ) ) ));
				float3 normalizeResult7_g121 = normalize( lerpResult84_g121 );
				float dotResult9_g121 = dot( normalizeResult7_g121 , float3(0,1,0) );
				float temp_output_12_0_g121 = acos( dotResult9_g121 );
				float NaNPrevention21_g121 = step( 0.01 , abs( ( temp_output_12_0_g121 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g121 = lerp( float3(1,0,0) , appendResult24_g121 , NaNPrevention21_g121);
				float4 transform28_g121 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g121 , 0.0 ));
				float3 normalizeResult49_g121 = normalize( (transform28_g121).xyz );
				float3 RotationAxis30_g121 = normalizeResult49_g121;
				float SlopeCorrectionOffset150 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude146 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g121 = ( saturate( ( (0.0 + ((StaticNoise120).x - 0.0) * (SlopeCorrectionOffset150 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude146 ) ) * temp_output_12_0_g121 );
				float3 rotatedValue35_g121 = RotateAroundAxis( MB_LocalPivot139, ( v.vertex.xyz + MainBending89_g121 ), RotationAxis30_g121, RotationAngle29_g121 );
				float3 lerpResult52_g121 = lerp( MainBending89_g121 , ( rotatedValue35_g121 - v.vertex.xyz ) , NaNPrevention21_g121);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch159 = lerpResult52_g121;
				#else
				float3 staticSwitch159 = temp_output_149_0;
				#endif
				float3 LocalVertexOffset163 = staticSwitch159;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset163;
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

				float2 uv_MainTex = IN.ase_texcoord2.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float4 Albedo162 = tex2D( _MainTex, uv_MainTex );
				
				
				float3 Albedo = Albedo162.rgb;
				float3 Emission = 0;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

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

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGBA

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define ASE_SRP_VERSION 70301

			#pragma enable_d3d11_debug_symbols
			#pragma prefer_hlslcc gles
			#pragma exclude_renderers d3d11_9x

			#pragma vertex vert
			#pragma fragment frag

			#define SHADERPASS_2D

			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Core.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/Lighting.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/Color.hlsl"
			#include "Packages/com.unity.render-pipelines.core/ShaderLibrary/UnityInstancing.hlsl"
			#include "Packages/com.unity.render-pipelines.universal/ShaderLibrary/ShaderGraphFunctions.hlsl"
			
			#define ASE_NEEDS_VERT_POSITION
			#pragma shader_feature_local _ENABLESLOPECORRECTION_ON
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setup
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
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
			float _Metallic;
			float _Smoothness;
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
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float lerpResult99 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection119 = lerpResult99;
				float MB_WindDirectionVariation110 = _MBWindDirOffset;
				float4 transform1_g122 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord125 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult136 = (float3(texCoord125.x , 0.0 , texCoord125.y));
				float3 MB_LocalPivot139 = -appendResult136;
				float4 transform2_g122 = mul(GetObjectToWorldMatrix(),float4( MB_LocalPivot139 , 0.0 ));
				float2 UVs27_g115 = ( (transform1_g122).xz + (transform2_g122).xz );
				float4 temp_output_24_0_g115 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g115 = (temp_output_24_0_g115).zw;
				float2 panner7_g115 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise121 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g115 * AnimatedNoiseTilling29_g115 ) + panner7_g115 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection119 + ( MB_WindDirectionVariation110 * (-1.0 + ((AnimatedNoise121).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis138 = normalizeResult34_g118;
				float3 RotationAxis56_g120 = MB_RotationAxis138;
				float MB_Amplitude109 = _MBAmplitude;
				float MB_AmplitudeOffset115 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g115 = (temp_output_24_0_g115).xy;
				float4 StaticNoise120 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g115 * StaticNoileTilling28_g115 ), 0, 0.0) );
				float4 StaticWorldNoise31_g119 = StaticNoise120;
				float4 transform8_g119 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency118 = _MBFrequency;
				float MB_FrequencyOffset113 = _MBFrequencyOffset;
				float PhaseShift112 = v.ase_color.a;
				float MB_Phase117 = _MBPhase;
				float MB_DefaultBending111 = _MBDefaultBending;
				float MB_MaxHeight114 = _MBMaxHeight;
				float MB_RotationAngle140 = radians( ( ( ( ( MB_Amplitude109 + ( MB_AmplitudeOffset115 * (StaticWorldNoise31_g119).x ) ) * sin( ( ( ( transform8_g119.x + transform8_g119.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency118 + ( MB_FrequencyOffset113 * (StaticWorldNoise31_g119).x ) ) ) + ( ( 2.0 * PI ) * PhaseShift112 ) ) ) * MB_Phase117 ) ) ) + MB_DefaultBending111 ) * ( v.vertex.xyz.y / MB_MaxHeight114 ) ) );
				float RotationAngle54_g120 = MB_RotationAngle140;
				float3 PivotPoint60_g120 = MB_LocalPivot139;
				float3 break62_g120 = PivotPoint60_g120;
				float3 appendResult45_g120 = (float3(break62_g120.x , v.vertex.xyz.y , break62_g120.z));
				float3 rotatedValue30_g120 = RotateAroundAxis( appendResult45_g120, v.vertex.xyz, RotationAxis56_g120, RotationAngle54_g120 );
				float3 rotatedValue34_g120 = RotateAroundAxis( PivotPoint60_g120, ( rotatedValue30_g120 + float3( 0,0,0 ) ), RotationAxis56_g120, RotationAngle54_g120 );
				float3 temp_output_149_0 = ( ( rotatedValue34_g120 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g121 = temp_output_149_0;
				float3 appendResult15_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g121 , 0.0 ));
				float4 break20_g121 = transform17_g121;
				float3 appendResult24_g121 = (float3(-break20_g121.z , 0.0 , break20_g121.x));
				float3 appendResult3_g121 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g121 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g121 , 0.0 ));
				float3 lerpResult84_g121 = lerp( float3(0,1,0) , (transform4_g121).xyz , step( 1E-06 , ( abs( transform4_g121.x ) + abs( transform4_g121.z ) ) ));
				float3 normalizeResult7_g121 = normalize( lerpResult84_g121 );
				float dotResult9_g121 = dot( normalizeResult7_g121 , float3(0,1,0) );
				float temp_output_12_0_g121 = acos( dotResult9_g121 );
				float NaNPrevention21_g121 = step( 0.01 , abs( ( temp_output_12_0_g121 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g121 = lerp( float3(1,0,0) , appendResult24_g121 , NaNPrevention21_g121);
				float4 transform28_g121 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g121 , 0.0 ));
				float3 normalizeResult49_g121 = normalize( (transform28_g121).xyz );
				float3 RotationAxis30_g121 = normalizeResult49_g121;
				float SlopeCorrectionOffset150 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude146 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g121 = ( saturate( ( (0.0 + ((StaticNoise120).x - 0.0) * (SlopeCorrectionOffset150 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude146 ) ) * temp_output_12_0_g121 );
				float3 rotatedValue35_g121 = RotateAroundAxis( MB_LocalPivot139, ( v.vertex.xyz + MainBending89_g121 ), RotationAxis30_g121, RotationAngle29_g121 );
				float3 lerpResult52_g121 = lerp( MainBending89_g121 , ( rotatedValue35_g121 - v.vertex.xyz ) , NaNPrevention21_g121);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch159 = lerpResult52_g121;
				#else
				float3 staticSwitch159 = temp_output_149_0;
				#endif
				float3 LocalVertexOffset163 = staticSwitch159;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset163;
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

				float2 uv_MainTex = IN.ase_texcoord2.xy * _MainTex_ST.xy + _MainTex_ST.zw;
				float4 Albedo162 = tex2D( _MainTex, uv_MainTex );
				
				
				float3 Albedo = Albedo162.rgb;
				float Alpha = 1;
				float AlphaClipThreshold = 0.5;

				half4 color = half4( Albedo, Alpha );

				#ifdef _ALPHATEST_ON
					clip(Alpha - AlphaClipThreshold);
				#endif

				return color;
			}
			ENDHLSL
		}
		
	}
	/*ase_lod*/
	CustomEditor "LowPolyReedmace_MaterialInspector"
	
	
}
/*ASEBEGIN
Version=18600
0;72.57143;1496;810;5700.692;825.8368;3.25912;True;False
Node;AmplifyShaderEditor.CommentaryNode;91;-4092.778,512.1687;Inherit;False;891.4651;389.8646;;6;112;98;139;168;136;125;Vertex Colors and UVs Baked Data;1,1,1,1;0;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;125;-4034.098,739.8298;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;136;-3777.771,747.2927;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NegateNode;168;-3593.364,745.0227;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;87;-4606.092,-509.5593;Inherit;False;1400.904;764.9824;;8;170;169;121;120;107;94;95;96;World Space Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;139;-3421.299,741.2927;Inherit;False;MB_LocalPivot;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;170;-4539.745,-373.2383;Inherit;False;139;MB_LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;88;-5757.698,-1086.176;Inherit;False;888.7255;1337.645;;25;150;146;142;141;119;118;117;115;114;113;111;110;109;108;106;105;104;103;102;101;100;99;97;93;92;Material Properties;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;93;-5704.681,-256.3602;Inherit;False;Property;_MBWindDirBlend;MB Wind Dir Blend;14;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;95;-4353.681,-83.24121;Inherit;False;Property;_NoiseTextureTilling;Noise Tilling - Static (XY), Animated (ZW);4;0;Create;False;0;0;False;0;False;1,1,1,1;1,1,1,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;97;-5707.681,-341.3602;Inherit;False;Global;MBGlobalWindDir;MB Global Wind Dir;28;1;[HideInInspector];Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;96;-4273.681,105.7598;Float;False;Property;_NoisePannerSpeed;Noise Panner Speed;5;0;Create;True;0;0;False;0;False;0.05,0.03;0.08,0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TexturePropertyNode;94;-4301.293,-289.4453;Inherit;True;Property;_NoiseTexture;Noise Texture;3;1;[NoScaleOffset];Create;True;0;0;False;0;False;512fa11ad89d84543ad8d6c8d9cb6743;512fa11ad89d84543ad8d6c8d9cb6743;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.FunctionNode;169;-4309.745,-370.2383;Inherit;False;WorldSpaceUVs - NHP;-1;;122;88a2e8a391a04e241878bdb87d9283a3;0;1;6;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;92;-5707.496,-434.4222;Float;False;Property;_MBWindDir;MB Wind Dir;12;0;Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;106;-5709.418,-635.5123;Inherit;False;Property;_MBFrequencyOffset;MB Frequency Offset;10;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;101;-5707.496,-547.4222;Float;False;Property;_MBPhase;MB Phase;11;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;107;-3965.864,-175.3833;Inherit;False;WorldSpaceNoise - NHP;-1;;115;af5fa9ff24e18344ebcc05b64d296c57;0;4;22;FLOAT2;0,0;False;20;SAMPLER2D;;False;24;FLOAT4;1,1,1,1;False;19;FLOAT2;0.1,0.1;False;2;COLOR;0;COLOR;16
Node;AmplifyShaderEditor.RangedFloatNode;100;-5711.796,-1016.122;Float;False;Property;_MBDefaultBending;MB Default Bending;6;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;105;-5711.796,-728.1234;Float;False;Property;_MBFrequency;MB Frequency;9;0;Create;True;0;0;False;0;False;1.11;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;103;-5697.042,-46.27715;Inherit;False;Property;_MBMaxHeight;MB Max Height;15;0;Create;True;0;0;False;0;False;10;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;104;-5697.863,-145.2362;Float;False;Property;_MBWindDirOffset;MB Wind Dir Offset;13;0;Create;True;0;0;False;0;False;20;0;0;180;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;99;-5397.681,-362.3602;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;102;-5711.796,-920.1234;Float;False;Property;_MBAmplitude;MB Amplitude;7;0;Create;True;0;0;False;0;False;1.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;108;-5711.796,-823.1234;Float;False;Property;_MBAmplitudeOffset;MB Amplitude Offset;8;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;98;-4037.618,569.7148;Inherit;False;VertexColorData - NHP;-1;;116;0242ce46c610b224e91bc03a7bf52b77;0;1;17;FLOAT;0;False;3;FLOAT3;19;FLOAT3;0;FLOAT;18
Node;AmplifyShaderEditor.RegisterLocalVarNode;110;-5208.862,-145.2362;Inherit;False;MB_WindDirectionVariation;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;113;-5171.419,-635.4542;Inherit;False;MB_FrequencyOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;120;-3454.728,-214.0313;Inherit;False;StaticNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;118;-5137.695,-728.2232;Float;False;MB_Frequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;109;-5137.695,-920.2232;Float;False;MB_Amplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;121;-3452.781,-98.70923;Inherit;False;AnimatedNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;117;-5133.396,-547.5223;Inherit;False;MB_Phase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;116;-2940.594,-894.6973;Inherit;False;2940.38;1146.592;;28;163;159;157;154;151;156;153;149;145;144;143;140;138;0;137;135;123;124;130;126;122;134;129;133;127;128;132;131;Main Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;114;-5129.042,-45.27715;Inherit;False;MB_MaxHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;115;-5169.695,-824.2232;Inherit;False;MB_AmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;119;-5151.495,-368.9723;Float;False;MB_WindDirection;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;111;-5169.695,-1016.222;Float;False;MB_DefaultBending;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;112;-3428.118,605.2465;Float;False;PhaseShift;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;132;-2858.107,-164.6672;Inherit;False;113;MB_FrequencyOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;133;-2823.835,-437.6973;Inherit;False;109;MB_Amplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;129;-2855.835,-522.6973;Inherit;False;111;MB_DefaultBending;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;126;-2905.178,-741.9922;Inherit;False;110;MB_WindDirectionVariation;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;134;-2823.835,-255.6973;Inherit;False;118;MB_Frequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;124;-2833.764,92.47583;Inherit;False;114;MB_MaxHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;130;-2860.835,-344.6973;Inherit;False;115;MB_AmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;123;-2806.835,177.3027;Inherit;False;120;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;127;-2847.677,-825.1592;Inherit;False;119;MB_WindDirection;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;131;-2808.99,10.88574;Inherit;False;112;PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;122;-2830.094,-661.5933;Inherit;False;121;AnimatedNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;128;-2803.835,-76.69727;Inherit;False;117;MB_Phase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;137;-2459.866,-757.6763;Inherit;False;RotationAxis - NHP;-1;;118;b90648f17dcc4bc449d46e8cf04564ff;0;3;20;FLOAT;0;False;19;FLOAT;0;False;18;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;135;-2471.683,-283.9492;Inherit;False;RotationAngle - NHP;-1;;119;87b0b7c0fc8f1424db43b84d20c2e79b;0;9;36;FLOAT;0;False;35;FLOAT;0;False;34;FLOAT;1;False;28;FLOAT;1;False;47;FLOAT;0;False;29;FLOAT;1;False;46;FLOAT;0;False;42;FLOAT;0;False;27;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;141;-5694.863,67.76385;Inherit;False;Property;_SlopeCorrectionMagnitude;Slope Correction Magnitude;17;0;Create;True;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;138;-2071.68,-759.2493;Inherit;False;MB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;142;-5695.538,163.7077;Inherit;False;Property;_SlopeCorrectionOffset;Slope Correction Offset;18;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;140;-2068.241,-290.3093;Float;False;MB_RotationAngle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;150;-5172.689,164.7167;Inherit;False;SlopeCorrectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;143;-1739.878,-502.1756;Inherit;False;140;MB_RotationAngle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;144;-1726.878,-591.1757;Inherit;False;138;MB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;145;-1717.331,-414.5196;Inherit;False;139;MB_LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;147;-1284.431,-1534.969;Inherit;False;1281.093;382.0935;;4;162;160;158;152;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;146;-5198.862,67.76385;Inherit;False;SlopeCorrectionMagnitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;152;-1194.862,-1426.661;Float;True;Property;_MainTex;Main Texture;2;1;[NoScaleOffset];Create;False;0;0;False;1;Header(Textures);False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.GetLocalVarNode;153;-1347.659,-359.1646;Inherit;False;146;SlopeCorrectionMagnitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;151;-1279.164,-104.3878;Inherit;False;139;MB_LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;156;-1322.82,-276.4746;Inherit;False;150;SlopeCorrectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;149;-1443.771,-515.7417;Inherit;False;MainBending - NHP;-1;;120;01dba1f3bc33e4b4fa301d2180819576;0;4;55;FLOAT3;0,0,0;False;53;FLOAT;0;False;59;FLOAT3;0,0,0;False;58;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;154;-1262.82,-192.4747;Inherit;False;120;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;158;-918.2159,-1344.292;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;157;-986.9278,-318.2566;Inherit;False;SlopeCorrection - NHP;-1;;121;af38de3ca0adf3c4ba9b6a3dd482959e;0;5;87;FLOAT3;0,0,0;False;42;FLOAT;1;False;92;FLOAT;0;False;93;FLOAT4;0,0,0,0;False;41;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;160;-629.1165,-1425.541;Inherit;True;Property;_MainTexture;Main Texture;0;1;[NoScaleOffset];Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StaticSwitch;159;-550.0536,-517.1216;Float;False;Property;_EnableSlopeCorrection;Enable Slope Correction;16;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;161;386.77,-769.3876;Inherit;False;634.4954;509.4399;;5;165;167;164;1;166;Master Node;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;162;-286.1244,-1424.505;Float;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;163;-241.894,-517.1807;Float;False;LocalVertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;166;502.0508,-701.7978;Inherit;False;162;Albedo;1;0;OBJECT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;167;416.9339,-515.1292;Inherit;False;Property;_Smoothness;Smoothness;1;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;165;445.6409,-387.8094;Inherit;False;163;LocalVertexOffset;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;164;416.9339,-602.1292;Inherit;False;Property;_Metallic;Metallic;0;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;734.345,-638.2534;Float;False;True;-1;2;LowPolyReedmace_MaterialInspector;0;2;Nicrom/NHP/ASE//Vegetation Studio/Low Poly Reedmace;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;17;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;5;Include;;False;;Native;Pragma;multi_compile GPU_FRUSTUM_ON __;False;;Custom;Pragma;instancing_options procedural:setup;False;;Custom;Pragma;instancing_options procedural:setup forwardadd;False;;Custom;Include;VS_indirect.cginc;False;;Custom;;0;0;Standard;36;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;1;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;-74,15;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;False;False;False;False;0;False;-1;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;136;0;125;1
WireConnection;136;2;125;2
WireConnection;168;0;136;0
WireConnection;139;0;168;0
WireConnection;169;6;170;0
WireConnection;107;22;169;0
WireConnection;107;20;94;0
WireConnection;107;24;95;0
WireConnection;107;19;96;0
WireConnection;99;0;92;0
WireConnection;99;1;97;0
WireConnection;99;2;93;0
WireConnection;110;0;104;0
WireConnection;113;0;106;0
WireConnection;120;0;107;0
WireConnection;118;0;105;0
WireConnection;109;0;102;0
WireConnection;121;0;107;16
WireConnection;117;0;101;0
WireConnection;114;0;103;0
WireConnection;115;0;108;0
WireConnection;119;0;99;0
WireConnection;111;0;100;0
WireConnection;112;0;98;18
WireConnection;137;20;127;0
WireConnection;137;19;126;0
WireConnection;137;18;122;0
WireConnection;135;36;129;0
WireConnection;135;35;133;0
WireConnection;135;34;130;0
WireConnection;135;28;134;0
WireConnection;135;47;132;0
WireConnection;135;29;128;0
WireConnection;135;46;131;0
WireConnection;135;42;124;0
WireConnection;135;27;123;0
WireConnection;138;0;137;0
WireConnection;140;0;135;0
WireConnection;150;0;142;0
WireConnection;146;0;141;0
WireConnection;149;55;144;0
WireConnection;149;53;143;0
WireConnection;149;59;145;0
WireConnection;158;2;152;0
WireConnection;157;87;149;0
WireConnection;157;42;153;0
WireConnection;157;92;156;0
WireConnection;157;93;154;0
WireConnection;157;41;151;0
WireConnection;160;0;152;0
WireConnection;160;1;158;0
WireConnection;159;1;149;0
WireConnection;159;0;157;0
WireConnection;162;0;160;0
WireConnection;163;0;159;0
WireConnection;1;0;166;0
WireConnection;1;3;164;0
WireConnection;1;4;167;0
WireConnection;1;8;165;0
ASEEND*/
//CHKSM=9DE9CB6D315006BDF8C38074C53D640D8A4B971C