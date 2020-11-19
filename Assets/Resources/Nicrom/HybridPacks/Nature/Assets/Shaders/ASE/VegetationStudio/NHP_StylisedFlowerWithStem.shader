// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Nicrom/NHP/ASE/Vegetation Studio/Stylised Flower With Stem"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[ASEBegin]_FlowerColor1("Flower Color 1", Color) = (0.7843137,0.454902,0.1411765,1)
		_FlowerColor2("Flower Color 2", Color) = (0.8980392,0.9529412,1,1)
		_ColorBlendStart("Color Blend Start", Range( 0 , 1)) = 0.1
		_ColorBlendEnd("Color Blend End", Range( 0 , 1)) = 0.15
		_StemColor("Stem Color", Color) = (0.3960784,0.5647059,0.1019608,1)
		[NoScaleOffset]_MainTex("Flower Texture", 2D) = "white" {}
		[NoScaleOffset]_StemTexture("Stem Texture", 2D) = "white" {}
		_AlphaCutoff("Alpha Cutoff", Range( 0 , 1)) = 0.5
		_MBDefaultBending("MB Default Bending", Float) = 0
		_MBAmplitude("MB Amplitude", Float) = 1.5
		_MBAmplitudeOffset("MB Amplitude Offset", Float) = 2
		_MBFrequency("MB Frequency", Float) = 1.11
		_MBFrequencyOffset("MB Frequency Offset", Float) = 0
		_MBPhase("MB Phase", Float) = 1
		_MBWindDir("MB Wind Dir", Range( 0 , 360)) = 0
		_MBWindDirOffset("MB Wind Dir Offset", Range( 0 , 180)) = 20
		_MBWindDirBlend("MB Wind Dir Blend", Range( 0 , 1)) = 0
		_MBMaxHeight("MB Max Height", Float) = 1
		[Toggle(_ENABLEHORIZONTALBENDING_ON)] _EnableHorizontalBending("Enable Horizontal Bending", Float) = 1
		_DBHorizontalAmplitude("DB Horizontal Amplitude", Float) = 2
		_DBHorizontalFrequency("DB Horizontal Frequency", Float) = 1.16
		_DBHorizontalPhase("DB Horizontal Phase", Float) = 1
		_DBHorizontalMaxRadius("DB Horizontal Max Radius", Float) = 0.05
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
			ColorMask RGB
			

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setupScale
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
			float4 _NoiseTextureTilling;
			float4 _FlowerColor2;
			float4 _FlowerColor1;
			float4 _StemColor;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendEnd;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBHorizontalAmplitude;
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
			sampler2D _StemTexture;
			SAMPLER(sampler_MainTex);
			SAMPLER(sampler_StemTexture);


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

				float lerpResult181 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection205 = lerpResult181;
				float MB_WindDirectionOffset204 = _MBWindDirOffset;
				float4 transform1_g134 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord152 = v.texcoord1.xyzw.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult153 = (float3(texCoord152.x , 0.0 , texCoord152.y));
				float3 LocalPivot154 = -appendResult153;
				float4 transform2_g134 = mul(GetObjectToWorldMatrix(),float4( LocalPivot154 , 0.0 ));
				float2 UVs27_g135 = ( (transform1_g134).xz + (transform2_g134).xz );
				float4 temp_output_24_0_g135 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g135 = (temp_output_24_0_g135).zw;
				float2 panner7_g135 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g135 * AnimatedNoiseTilling29_g135 ) + panner7_g135 ), 0, 0.0) );
				float temp_output_11_0_g141 = radians( ( ( MB_WindDirection205 + ( MB_WindDirectionOffset204 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g141 = (float3(cos( temp_output_11_0_g141 ) , 0.0 , sin( temp_output_11_0_g141 )));
				float4 transform15_g141 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g141 , 0.0 ));
				float3 normalizeResult34_g141 = normalize( (transform15_g141).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g141;
				float3 RotationAxis56_g143 = MB_RotationAxis246;
				float MB_Amplitude207 = _MBAmplitude;
				float MB_AmplitudeOffset218 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g135 = (temp_output_24_0_g135).xy;
				float4 StaticNoise202 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g135 * StaticNoileTilling28_g135 ), 0, 0.0) );
				float4 StaticWorldNoise31_g142 = StaticNoise202;
				float4 transform8_g142 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency215 = _MBFrequency;
				float MB_FrequencyOffset209 = _MBFrequencyOffset;
				float DB_PhaseShift194 = v.ase_color.a;
				float MB_Phase208 = _MBPhase;
				float MB_DefaultBending206 = _MBDefaultBending;
				float MB_MaxHeight211 = _MBMaxHeight;
				float MB_RotationAngle245 = radians( ( ( ( ( MB_Amplitude207 + ( MB_AmplitudeOffset218 * (StaticWorldNoise31_g142).x ) ) * sin( ( ( ( transform8_g142.x + transform8_g142.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency215 + ( MB_FrequencyOffset209 * (StaticWorldNoise31_g142).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift194 ) ) ) * MB_Phase208 ) ) ) + MB_DefaultBending206 ) * ( v.vertex.xyz.y / MB_MaxHeight211 ) ) );
				float RotationAngle54_g143 = MB_RotationAngle245;
				float3 PivotPoint60_g143 = LocalPivot154;
				float3 break62_g143 = PivotPoint60_g143;
				float3 appendResult45_g143 = (float3(break62_g143.x , v.vertex.xyz.y , break62_g143.z));
				float3 rotatedValue30_g143 = RotateAroundAxis( appendResult45_g143, v.vertex.xyz, RotationAxis56_g143, RotationAngle54_g143 );
				float DB_HorizontalAmplitude189 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency193 = _DBHorizontalFrequency;
				float Frequency41_g136 = DB_HorizontalFrequency193;
				float4 transform5_g136 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase187 = _DBHorizontalPhase;
				float3 PivotPoint49_g136 = LocalPivot154;
				float3 break52_g136 = PivotPoint49_g136;
				float3 appendResult20_g136 = (float3(break52_g136.x , v.vertex.xyz.y , break52_g136.z));
				float DB_HorizontalMaxRadius192 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g136 = RotateAroundAxis( PivotPoint49_g136, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude189 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g136 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift194 ) ) ) + ( ( ( transform5_g136.x + transform5_g136.z ) + ( ( _TimeParameters.x ) * Frequency41_g136 ) ) * DB_HorizontalPhase187 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g136 ) / DB_HorizontalMaxRadius192 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch242 = ( ( rotatedValue33_g136 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch242 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset248 = staticSwitch242;
				float3 rotatedValue34_g143 = RotateAroundAxis( PivotPoint60_g143, ( rotatedValue30_g143 + DB_VertexOffset248 ), RotationAxis56_g143, RotationAngle54_g143 );
				float3 temp_output_265_0 = ( ( rotatedValue34_g143 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g144 = temp_output_265_0;
				float3 appendResult15_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g144 , 0.0 ));
				float4 break20_g144 = transform17_g144;
				float3 appendResult24_g144 = (float3(-break20_g144.z , 0.0 , break20_g144.x));
				float3 appendResult3_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g144 , 0.0 ));
				float3 lerpResult84_g144 = lerp( float3(0,1,0) , (transform4_g144).xyz , step( 1E-06 , ( abs( transform4_g144.x ) + abs( transform4_g144.z ) ) ));
				float3 normalizeResult7_g144 = normalize( lerpResult84_g144 );
				float dotResult9_g144 = dot( normalizeResult7_g144 , float3(0,1,0) );
				float temp_output_12_0_g144 = acos( dotResult9_g144 );
				float NaNPrevention21_g144 = step( 0.01 , abs( ( temp_output_12_0_g144 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g144 = lerp( float3(1,0,0) , appendResult24_g144 , NaNPrevention21_g144);
				float4 transform28_g144 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g144 , 0.0 ));
				float3 normalizeResult49_g144 = normalize( (transform28_g144).xyz );
				float3 RotationAxis30_g144 = normalizeResult49_g144;
				float SlopeCorrectionOffset269 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude268 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g144 = ( saturate( ( (0.0 + ((StaticNoise202).x - 0.0) * (SlopeCorrectionOffset269 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude268 ) ) * temp_output_12_0_g144 );
				float3 rotatedValue35_g144 = RotateAroundAxis( LocalPivot154, ( v.vertex.xyz + MainBending89_g144 ), RotationAxis30_g144, RotationAngle29_g144 );
				float3 lerpResult52_g144 = lerp( MainBending89_g144 , ( rotatedValue35_g144 - v.vertex.xyz ) , NaNPrevention21_g144);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch282 = lerpResult52_g144;
				#else
				float3 staticSwitch282 = temp_output_265_0;
				#endif
				float3 LocalVertexOffset285 = staticSwitch282;
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset285;
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

				float2 uv_MainTex253 = IN.ase_texcoord7.xy;
				float4 tex2DNode253 = tex2D( _MainTex, uv_MainTex253 );
				float2 uv_StemTexture260 = IN.ase_texcoord7.xy;
				float4 tex2DNode260 = tex2D( _StemTexture, uv_StemTexture260 );
				float TextureMask250 = step( 1.5 , IN.ase_texcoord7.xy.x );
				float4 lerpResult267 = lerp( tex2DNode253 , tex2DNode260 , TextureMask250);
				float4 TextureColor272 = lerpResult267;
				float DistanceToCenter177 = distance( float2( 0.5,0.5 ) , IN.ase_texcoord7.xy );
				float ColorBlendStart173 = _ColorBlendStart;
				float ColorBlendEnd190 = _ColorBlendEnd;
				float4 lerpResult255 = lerp( _FlowerColor1 , _FlowerColor2 , ( saturate( ( ( DistanceToCenter177 - ColorBlendStart173 ) / ColorBlendEnd190 ) ) * step( ColorBlendStart173 , DistanceToCenter177 ) ));
				float4 lerpResult264 = lerp( lerpResult255 , _StemColor , TextureMask250);
				float4 Color274 = lerpResult264;
				float4 Albedo284 = ( TextureColor272 * Color274 );
				
				float lerpResult280 = lerp( tex2DNode253.a , tex2DNode260.a , TextureMask250);
				float Opacity283 = lerpResult280;
				
				float3 Albedo = Albedo284.rgb;
				float3 Normal = float3(0, 0, 1);
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = 0.0;
				float Smoothness = 0.0;
				float Occlusion = 1;
				float Alpha = Opacity283;
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setupScale
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


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
			float4 _NoiseTextureTilling;
			float4 _FlowerColor2;
			float4 _FlowerColor1;
			float4 _StemColor;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendEnd;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBHorizontalAmplitude;
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
			sampler2D _StemTexture;
			SAMPLER(sampler_StemTexture);


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

				float lerpResult181 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection205 = lerpResult181;
				float MB_WindDirectionOffset204 = _MBWindDirOffset;
				float4 transform1_g134 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord152 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult153 = (float3(texCoord152.x , 0.0 , texCoord152.y));
				float3 LocalPivot154 = -appendResult153;
				float4 transform2_g134 = mul(GetObjectToWorldMatrix(),float4( LocalPivot154 , 0.0 ));
				float2 UVs27_g135 = ( (transform1_g134).xz + (transform2_g134).xz );
				float4 temp_output_24_0_g135 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g135 = (temp_output_24_0_g135).zw;
				float2 panner7_g135 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g135 * AnimatedNoiseTilling29_g135 ) + panner7_g135 ), 0, 0.0) );
				float temp_output_11_0_g141 = radians( ( ( MB_WindDirection205 + ( MB_WindDirectionOffset204 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g141 = (float3(cos( temp_output_11_0_g141 ) , 0.0 , sin( temp_output_11_0_g141 )));
				float4 transform15_g141 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g141 , 0.0 ));
				float3 normalizeResult34_g141 = normalize( (transform15_g141).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g141;
				float3 RotationAxis56_g143 = MB_RotationAxis246;
				float MB_Amplitude207 = _MBAmplitude;
				float MB_AmplitudeOffset218 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g135 = (temp_output_24_0_g135).xy;
				float4 StaticNoise202 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g135 * StaticNoileTilling28_g135 ), 0, 0.0) );
				float4 StaticWorldNoise31_g142 = StaticNoise202;
				float4 transform8_g142 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency215 = _MBFrequency;
				float MB_FrequencyOffset209 = _MBFrequencyOffset;
				float DB_PhaseShift194 = v.ase_color.a;
				float MB_Phase208 = _MBPhase;
				float MB_DefaultBending206 = _MBDefaultBending;
				float MB_MaxHeight211 = _MBMaxHeight;
				float MB_RotationAngle245 = radians( ( ( ( ( MB_Amplitude207 + ( MB_AmplitudeOffset218 * (StaticWorldNoise31_g142).x ) ) * sin( ( ( ( transform8_g142.x + transform8_g142.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency215 + ( MB_FrequencyOffset209 * (StaticWorldNoise31_g142).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift194 ) ) ) * MB_Phase208 ) ) ) + MB_DefaultBending206 ) * ( v.vertex.xyz.y / MB_MaxHeight211 ) ) );
				float RotationAngle54_g143 = MB_RotationAngle245;
				float3 PivotPoint60_g143 = LocalPivot154;
				float3 break62_g143 = PivotPoint60_g143;
				float3 appendResult45_g143 = (float3(break62_g143.x , v.vertex.xyz.y , break62_g143.z));
				float3 rotatedValue30_g143 = RotateAroundAxis( appendResult45_g143, v.vertex.xyz, RotationAxis56_g143, RotationAngle54_g143 );
				float DB_HorizontalAmplitude189 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency193 = _DBHorizontalFrequency;
				float Frequency41_g136 = DB_HorizontalFrequency193;
				float4 transform5_g136 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase187 = _DBHorizontalPhase;
				float3 PivotPoint49_g136 = LocalPivot154;
				float3 break52_g136 = PivotPoint49_g136;
				float3 appendResult20_g136 = (float3(break52_g136.x , v.vertex.xyz.y , break52_g136.z));
				float DB_HorizontalMaxRadius192 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g136 = RotateAroundAxis( PivotPoint49_g136, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude189 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g136 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift194 ) ) ) + ( ( ( transform5_g136.x + transform5_g136.z ) + ( ( _TimeParameters.x ) * Frequency41_g136 ) ) * DB_HorizontalPhase187 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g136 ) / DB_HorizontalMaxRadius192 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch242 = ( ( rotatedValue33_g136 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch242 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset248 = staticSwitch242;
				float3 rotatedValue34_g143 = RotateAroundAxis( PivotPoint60_g143, ( rotatedValue30_g143 + DB_VertexOffset248 ), RotationAxis56_g143, RotationAngle54_g143 );
				float3 temp_output_265_0 = ( ( rotatedValue34_g143 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g144 = temp_output_265_0;
				float3 appendResult15_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g144 , 0.0 ));
				float4 break20_g144 = transform17_g144;
				float3 appendResult24_g144 = (float3(-break20_g144.z , 0.0 , break20_g144.x));
				float3 appendResult3_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g144 , 0.0 ));
				float3 lerpResult84_g144 = lerp( float3(0,1,0) , (transform4_g144).xyz , step( 1E-06 , ( abs( transform4_g144.x ) + abs( transform4_g144.z ) ) ));
				float3 normalizeResult7_g144 = normalize( lerpResult84_g144 );
				float dotResult9_g144 = dot( normalizeResult7_g144 , float3(0,1,0) );
				float temp_output_12_0_g144 = acos( dotResult9_g144 );
				float NaNPrevention21_g144 = step( 0.01 , abs( ( temp_output_12_0_g144 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g144 = lerp( float3(1,0,0) , appendResult24_g144 , NaNPrevention21_g144);
				float4 transform28_g144 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g144 , 0.0 ));
				float3 normalizeResult49_g144 = normalize( (transform28_g144).xyz );
				float3 RotationAxis30_g144 = normalizeResult49_g144;
				float SlopeCorrectionOffset269 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude268 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g144 = ( saturate( ( (0.0 + ((StaticNoise202).x - 0.0) * (SlopeCorrectionOffset269 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude268 ) ) * temp_output_12_0_g144 );
				float3 rotatedValue35_g144 = RotateAroundAxis( LocalPivot154, ( v.vertex.xyz + MainBending89_g144 ), RotationAxis30_g144, RotationAngle29_g144 );
				float3 lerpResult52_g144 = lerp( MainBending89_g144 , ( rotatedValue35_g144 - v.vertex.xyz ) , NaNPrevention21_g144);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch282 = lerpResult52_g144;
				#else
				float3 staticSwitch282 = temp_output_265_0;
				#endif
				float3 LocalVertexOffset285 = staticSwitch282;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset285;
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

				float2 uv_MainTex253 = IN.ase_texcoord2.xy;
				float4 tex2DNode253 = tex2D( _MainTex, uv_MainTex253 );
				float2 uv_StemTexture260 = IN.ase_texcoord2.xy;
				float4 tex2DNode260 = tex2D( _StemTexture, uv_StemTexture260 );
				float TextureMask250 = step( 1.5 , IN.ase_texcoord2.xy.x );
				float lerpResult280 = lerp( tex2DNode253.a , tex2DNode260.a , TextureMask250);
				float Opacity283 = lerpResult280;
				
				float Alpha = Opacity283;
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setupScale
			#pragma instancing_options procedural:setup forwardadd
			#include "VS_indirect.cginc"


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
			float4 _NoiseTextureTilling;
			float4 _FlowerColor2;
			float4 _FlowerColor1;
			float4 _StemColor;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendEnd;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBHorizontalAmplitude;
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
			sampler2D _StemTexture;
			SAMPLER(sampler_StemTexture);


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

				float lerpResult181 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection205 = lerpResult181;
				float MB_WindDirectionOffset204 = _MBWindDirOffset;
				float4 transform1_g134 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord152 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult153 = (float3(texCoord152.x , 0.0 , texCoord152.y));
				float3 LocalPivot154 = -appendResult153;
				float4 transform2_g134 = mul(GetObjectToWorldMatrix(),float4( LocalPivot154 , 0.0 ));
				float2 UVs27_g135 = ( (transform1_g134).xz + (transform2_g134).xz );
				float4 temp_output_24_0_g135 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g135 = (temp_output_24_0_g135).zw;
				float2 panner7_g135 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g135 * AnimatedNoiseTilling29_g135 ) + panner7_g135 ), 0, 0.0) );
				float temp_output_11_0_g141 = radians( ( ( MB_WindDirection205 + ( MB_WindDirectionOffset204 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g141 = (float3(cos( temp_output_11_0_g141 ) , 0.0 , sin( temp_output_11_0_g141 )));
				float4 transform15_g141 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g141 , 0.0 ));
				float3 normalizeResult34_g141 = normalize( (transform15_g141).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g141;
				float3 RotationAxis56_g143 = MB_RotationAxis246;
				float MB_Amplitude207 = _MBAmplitude;
				float MB_AmplitudeOffset218 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g135 = (temp_output_24_0_g135).xy;
				float4 StaticNoise202 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g135 * StaticNoileTilling28_g135 ), 0, 0.0) );
				float4 StaticWorldNoise31_g142 = StaticNoise202;
				float4 transform8_g142 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency215 = _MBFrequency;
				float MB_FrequencyOffset209 = _MBFrequencyOffset;
				float DB_PhaseShift194 = v.ase_color.a;
				float MB_Phase208 = _MBPhase;
				float MB_DefaultBending206 = _MBDefaultBending;
				float MB_MaxHeight211 = _MBMaxHeight;
				float MB_RotationAngle245 = radians( ( ( ( ( MB_Amplitude207 + ( MB_AmplitudeOffset218 * (StaticWorldNoise31_g142).x ) ) * sin( ( ( ( transform8_g142.x + transform8_g142.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency215 + ( MB_FrequencyOffset209 * (StaticWorldNoise31_g142).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift194 ) ) ) * MB_Phase208 ) ) ) + MB_DefaultBending206 ) * ( v.vertex.xyz.y / MB_MaxHeight211 ) ) );
				float RotationAngle54_g143 = MB_RotationAngle245;
				float3 PivotPoint60_g143 = LocalPivot154;
				float3 break62_g143 = PivotPoint60_g143;
				float3 appendResult45_g143 = (float3(break62_g143.x , v.vertex.xyz.y , break62_g143.z));
				float3 rotatedValue30_g143 = RotateAroundAxis( appendResult45_g143, v.vertex.xyz, RotationAxis56_g143, RotationAngle54_g143 );
				float DB_HorizontalAmplitude189 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency193 = _DBHorizontalFrequency;
				float Frequency41_g136 = DB_HorizontalFrequency193;
				float4 transform5_g136 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase187 = _DBHorizontalPhase;
				float3 PivotPoint49_g136 = LocalPivot154;
				float3 break52_g136 = PivotPoint49_g136;
				float3 appendResult20_g136 = (float3(break52_g136.x , v.vertex.xyz.y , break52_g136.z));
				float DB_HorizontalMaxRadius192 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g136 = RotateAroundAxis( PivotPoint49_g136, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude189 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g136 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift194 ) ) ) + ( ( ( transform5_g136.x + transform5_g136.z ) + ( ( _TimeParameters.x ) * Frequency41_g136 ) ) * DB_HorizontalPhase187 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g136 ) / DB_HorizontalMaxRadius192 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch242 = ( ( rotatedValue33_g136 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch242 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset248 = staticSwitch242;
				float3 rotatedValue34_g143 = RotateAroundAxis( PivotPoint60_g143, ( rotatedValue30_g143 + DB_VertexOffset248 ), RotationAxis56_g143, RotationAngle54_g143 );
				float3 temp_output_265_0 = ( ( rotatedValue34_g143 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g144 = temp_output_265_0;
				float3 appendResult15_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g144 , 0.0 ));
				float4 break20_g144 = transform17_g144;
				float3 appendResult24_g144 = (float3(-break20_g144.z , 0.0 , break20_g144.x));
				float3 appendResult3_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g144 , 0.0 ));
				float3 lerpResult84_g144 = lerp( float3(0,1,0) , (transform4_g144).xyz , step( 1E-06 , ( abs( transform4_g144.x ) + abs( transform4_g144.z ) ) ));
				float3 normalizeResult7_g144 = normalize( lerpResult84_g144 );
				float dotResult9_g144 = dot( normalizeResult7_g144 , float3(0,1,0) );
				float temp_output_12_0_g144 = acos( dotResult9_g144 );
				float NaNPrevention21_g144 = step( 0.01 , abs( ( temp_output_12_0_g144 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g144 = lerp( float3(1,0,0) , appendResult24_g144 , NaNPrevention21_g144);
				float4 transform28_g144 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g144 , 0.0 ));
				float3 normalizeResult49_g144 = normalize( (transform28_g144).xyz );
				float3 RotationAxis30_g144 = normalizeResult49_g144;
				float SlopeCorrectionOffset269 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude268 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g144 = ( saturate( ( (0.0 + ((StaticNoise202).x - 0.0) * (SlopeCorrectionOffset269 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude268 ) ) * temp_output_12_0_g144 );
				float3 rotatedValue35_g144 = RotateAroundAxis( LocalPivot154, ( v.vertex.xyz + MainBending89_g144 ), RotationAxis30_g144, RotationAngle29_g144 );
				float3 lerpResult52_g144 = lerp( MainBending89_g144 , ( rotatedValue35_g144 - v.vertex.xyz ) , NaNPrevention21_g144);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch282 = lerpResult52_g144;
				#else
				float3 staticSwitch282 = temp_output_265_0;
				#endif
				float3 LocalVertexOffset285 = staticSwitch282;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset285;
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

				float2 uv_MainTex253 = IN.ase_texcoord2.xy;
				float4 tex2DNode253 = tex2D( _MainTex, uv_MainTex253 );
				float2 uv_StemTexture260 = IN.ase_texcoord2.xy;
				float4 tex2DNode260 = tex2D( _StemTexture, uv_StemTexture260 );
				float TextureMask250 = step( 1.5 , IN.ase_texcoord2.xy.x );
				float lerpResult280 = lerp( tex2DNode253.a , tex2DNode260.a , TextureMask250);
				float Opacity283 = lerpResult280;
				
				float Alpha = Opacity283;
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setupScale
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
			float4 _NoiseTextureTilling;
			float4 _FlowerColor2;
			float4 _FlowerColor1;
			float4 _StemColor;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendEnd;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBHorizontalAmplitude;
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
			sampler2D _StemTexture;
			SAMPLER(sampler_MainTex);
			SAMPLER(sampler_StemTexture);


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

				float lerpResult181 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection205 = lerpResult181;
				float MB_WindDirectionOffset204 = _MBWindDirOffset;
				float4 transform1_g134 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord152 = v.texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult153 = (float3(texCoord152.x , 0.0 , texCoord152.y));
				float3 LocalPivot154 = -appendResult153;
				float4 transform2_g134 = mul(GetObjectToWorldMatrix(),float4( LocalPivot154 , 0.0 ));
				float2 UVs27_g135 = ( (transform1_g134).xz + (transform2_g134).xz );
				float4 temp_output_24_0_g135 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g135 = (temp_output_24_0_g135).zw;
				float2 panner7_g135 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g135 * AnimatedNoiseTilling29_g135 ) + panner7_g135 ), 0, 0.0) );
				float temp_output_11_0_g141 = radians( ( ( MB_WindDirection205 + ( MB_WindDirectionOffset204 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g141 = (float3(cos( temp_output_11_0_g141 ) , 0.0 , sin( temp_output_11_0_g141 )));
				float4 transform15_g141 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g141 , 0.0 ));
				float3 normalizeResult34_g141 = normalize( (transform15_g141).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g141;
				float3 RotationAxis56_g143 = MB_RotationAxis246;
				float MB_Amplitude207 = _MBAmplitude;
				float MB_AmplitudeOffset218 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g135 = (temp_output_24_0_g135).xy;
				float4 StaticNoise202 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g135 * StaticNoileTilling28_g135 ), 0, 0.0) );
				float4 StaticWorldNoise31_g142 = StaticNoise202;
				float4 transform8_g142 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency215 = _MBFrequency;
				float MB_FrequencyOffset209 = _MBFrequencyOffset;
				float DB_PhaseShift194 = v.ase_color.a;
				float MB_Phase208 = _MBPhase;
				float MB_DefaultBending206 = _MBDefaultBending;
				float MB_MaxHeight211 = _MBMaxHeight;
				float MB_RotationAngle245 = radians( ( ( ( ( MB_Amplitude207 + ( MB_AmplitudeOffset218 * (StaticWorldNoise31_g142).x ) ) * sin( ( ( ( transform8_g142.x + transform8_g142.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency215 + ( MB_FrequencyOffset209 * (StaticWorldNoise31_g142).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift194 ) ) ) * MB_Phase208 ) ) ) + MB_DefaultBending206 ) * ( v.vertex.xyz.y / MB_MaxHeight211 ) ) );
				float RotationAngle54_g143 = MB_RotationAngle245;
				float3 PivotPoint60_g143 = LocalPivot154;
				float3 break62_g143 = PivotPoint60_g143;
				float3 appendResult45_g143 = (float3(break62_g143.x , v.vertex.xyz.y , break62_g143.z));
				float3 rotatedValue30_g143 = RotateAroundAxis( appendResult45_g143, v.vertex.xyz, RotationAxis56_g143, RotationAngle54_g143 );
				float DB_HorizontalAmplitude189 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency193 = _DBHorizontalFrequency;
				float Frequency41_g136 = DB_HorizontalFrequency193;
				float4 transform5_g136 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase187 = _DBHorizontalPhase;
				float3 PivotPoint49_g136 = LocalPivot154;
				float3 break52_g136 = PivotPoint49_g136;
				float3 appendResult20_g136 = (float3(break52_g136.x , v.vertex.xyz.y , break52_g136.z));
				float DB_HorizontalMaxRadius192 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g136 = RotateAroundAxis( PivotPoint49_g136, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude189 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g136 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift194 ) ) ) + ( ( ( transform5_g136.x + transform5_g136.z ) + ( ( _TimeParameters.x ) * Frequency41_g136 ) ) * DB_HorizontalPhase187 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g136 ) / DB_HorizontalMaxRadius192 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch242 = ( ( rotatedValue33_g136 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch242 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset248 = staticSwitch242;
				float3 rotatedValue34_g143 = RotateAroundAxis( PivotPoint60_g143, ( rotatedValue30_g143 + DB_VertexOffset248 ), RotationAxis56_g143, RotationAngle54_g143 );
				float3 temp_output_265_0 = ( ( rotatedValue34_g143 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g144 = temp_output_265_0;
				float3 appendResult15_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g144 , 0.0 ));
				float4 break20_g144 = transform17_g144;
				float3 appendResult24_g144 = (float3(-break20_g144.z , 0.0 , break20_g144.x));
				float3 appendResult3_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g144 , 0.0 ));
				float3 lerpResult84_g144 = lerp( float3(0,1,0) , (transform4_g144).xyz , step( 1E-06 , ( abs( transform4_g144.x ) + abs( transform4_g144.z ) ) ));
				float3 normalizeResult7_g144 = normalize( lerpResult84_g144 );
				float dotResult9_g144 = dot( normalizeResult7_g144 , float3(0,1,0) );
				float temp_output_12_0_g144 = acos( dotResult9_g144 );
				float NaNPrevention21_g144 = step( 0.01 , abs( ( temp_output_12_0_g144 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g144 = lerp( float3(1,0,0) , appendResult24_g144 , NaNPrevention21_g144);
				float4 transform28_g144 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g144 , 0.0 ));
				float3 normalizeResult49_g144 = normalize( (transform28_g144).xyz );
				float3 RotationAxis30_g144 = normalizeResult49_g144;
				float SlopeCorrectionOffset269 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude268 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g144 = ( saturate( ( (0.0 + ((StaticNoise202).x - 0.0) * (SlopeCorrectionOffset269 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude268 ) ) * temp_output_12_0_g144 );
				float3 rotatedValue35_g144 = RotateAroundAxis( LocalPivot154, ( v.vertex.xyz + MainBending89_g144 ), RotationAxis30_g144, RotationAngle29_g144 );
				float3 lerpResult52_g144 = lerp( MainBending89_g144 , ( rotatedValue35_g144 - v.vertex.xyz ) , NaNPrevention21_g144);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch282 = lerpResult52_g144;
				#else
				float3 staticSwitch282 = temp_output_265_0;
				#endif
				float3 LocalVertexOffset285 = staticSwitch282;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset285;
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

				float2 uv_MainTex253 = IN.ase_texcoord2.xy;
				float4 tex2DNode253 = tex2D( _MainTex, uv_MainTex253 );
				float2 uv_StemTexture260 = IN.ase_texcoord2.xy;
				float4 tex2DNode260 = tex2D( _StemTexture, uv_StemTexture260 );
				float TextureMask250 = step( 1.5 , IN.ase_texcoord2.xy.x );
				float4 lerpResult267 = lerp( tex2DNode253 , tex2DNode260 , TextureMask250);
				float4 TextureColor272 = lerpResult267;
				float DistanceToCenter177 = distance( float2( 0.5,0.5 ) , IN.ase_texcoord2.xy );
				float ColorBlendStart173 = _ColorBlendStart;
				float ColorBlendEnd190 = _ColorBlendEnd;
				float4 lerpResult255 = lerp( _FlowerColor1 , _FlowerColor2 , ( saturate( ( ( DistanceToCenter177 - ColorBlendStart173 ) / ColorBlendEnd190 ) ) * step( ColorBlendStart173 , DistanceToCenter177 ) ));
				float4 lerpResult264 = lerp( lerpResult255 , _StemColor , TextureMask250);
				float4 Color274 = lerpResult264;
				float4 Albedo284 = ( TextureColor272 * Color274 );
				
				float lerpResult280 = lerp( tex2DNode253.a , tex2DNode260.a , TextureMask250);
				float Opacity283 = lerpResult280;
				
				
				float3 Albedo = Albedo284.rgb;
				float3 Emission = 0;
				float Alpha = Opacity283;
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

		
		Pass
		{
			
			Name "Universal2D"
			Tags { "LightMode"="Universal2D" }

			Blend One Zero, One Zero
			ZWrite On
			ZTest LEqual
			Offset 0 , 0
			ColorMask RGB

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile_fog
			#define ASE_FOG 1
			#define _ALPHATEST_ON 1
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON
			#pragma multi_compile GPU_FRUSTUM_ON __
			#pragma instancing_options procedural:setupScale
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
			float4 _NoiseTextureTilling;
			float4 _FlowerColor2;
			float4 _FlowerColor1;
			float4 _StemColor;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendEnd;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBHorizontalAmplitude;
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
			sampler2D _StemTexture;
			SAMPLER(sampler_MainTex);
			SAMPLER(sampler_StemTexture);


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

				float lerpResult181 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection205 = lerpResult181;
				float MB_WindDirectionOffset204 = _MBWindDirOffset;
				float4 transform1_g134 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord152 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult153 = (float3(texCoord152.x , 0.0 , texCoord152.y));
				float3 LocalPivot154 = -appendResult153;
				float4 transform2_g134 = mul(GetObjectToWorldMatrix(),float4( LocalPivot154 , 0.0 ));
				float2 UVs27_g135 = ( (transform1_g134).xz + (transform2_g134).xz );
				float4 temp_output_24_0_g135 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g135 = (temp_output_24_0_g135).zw;
				float2 panner7_g135 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g135 * AnimatedNoiseTilling29_g135 ) + panner7_g135 ), 0, 0.0) );
				float temp_output_11_0_g141 = radians( ( ( MB_WindDirection205 + ( MB_WindDirectionOffset204 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g141 = (float3(cos( temp_output_11_0_g141 ) , 0.0 , sin( temp_output_11_0_g141 )));
				float4 transform15_g141 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g141 , 0.0 ));
				float3 normalizeResult34_g141 = normalize( (transform15_g141).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g141;
				float3 RotationAxis56_g143 = MB_RotationAxis246;
				float MB_Amplitude207 = _MBAmplitude;
				float MB_AmplitudeOffset218 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g135 = (temp_output_24_0_g135).xy;
				float4 StaticNoise202 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g135 * StaticNoileTilling28_g135 ), 0, 0.0) );
				float4 StaticWorldNoise31_g142 = StaticNoise202;
				float4 transform8_g142 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency215 = _MBFrequency;
				float MB_FrequencyOffset209 = _MBFrequencyOffset;
				float DB_PhaseShift194 = v.ase_color.a;
				float MB_Phase208 = _MBPhase;
				float MB_DefaultBending206 = _MBDefaultBending;
				float MB_MaxHeight211 = _MBMaxHeight;
				float MB_RotationAngle245 = radians( ( ( ( ( MB_Amplitude207 + ( MB_AmplitudeOffset218 * (StaticWorldNoise31_g142).x ) ) * sin( ( ( ( transform8_g142.x + transform8_g142.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency215 + ( MB_FrequencyOffset209 * (StaticWorldNoise31_g142).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift194 ) ) ) * MB_Phase208 ) ) ) + MB_DefaultBending206 ) * ( v.vertex.xyz.y / MB_MaxHeight211 ) ) );
				float RotationAngle54_g143 = MB_RotationAngle245;
				float3 PivotPoint60_g143 = LocalPivot154;
				float3 break62_g143 = PivotPoint60_g143;
				float3 appendResult45_g143 = (float3(break62_g143.x , v.vertex.xyz.y , break62_g143.z));
				float3 rotatedValue30_g143 = RotateAroundAxis( appendResult45_g143, v.vertex.xyz, RotationAxis56_g143, RotationAngle54_g143 );
				float DB_HorizontalAmplitude189 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency193 = _DBHorizontalFrequency;
				float Frequency41_g136 = DB_HorizontalFrequency193;
				float4 transform5_g136 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase187 = _DBHorizontalPhase;
				float3 PivotPoint49_g136 = LocalPivot154;
				float3 break52_g136 = PivotPoint49_g136;
				float3 appendResult20_g136 = (float3(break52_g136.x , v.vertex.xyz.y , break52_g136.z));
				float DB_HorizontalMaxRadius192 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g136 = RotateAroundAxis( PivotPoint49_g136, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude189 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g136 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift194 ) ) ) + ( ( ( transform5_g136.x + transform5_g136.z ) + ( ( _TimeParameters.x ) * Frequency41_g136 ) ) * DB_HorizontalPhase187 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g136 ) / DB_HorizontalMaxRadius192 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch242 = ( ( rotatedValue33_g136 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch242 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset248 = staticSwitch242;
				float3 rotatedValue34_g143 = RotateAroundAxis( PivotPoint60_g143, ( rotatedValue30_g143 + DB_VertexOffset248 ), RotationAxis56_g143, RotationAngle54_g143 );
				float3 temp_output_265_0 = ( ( rotatedValue34_g143 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g144 = temp_output_265_0;
				float3 appendResult15_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g144 , 0.0 ));
				float4 break20_g144 = transform17_g144;
				float3 appendResult24_g144 = (float3(-break20_g144.z , 0.0 , break20_g144.x));
				float3 appendResult3_g144 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g144 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g144 , 0.0 ));
				float3 lerpResult84_g144 = lerp( float3(0,1,0) , (transform4_g144).xyz , step( 1E-06 , ( abs( transform4_g144.x ) + abs( transform4_g144.z ) ) ));
				float3 normalizeResult7_g144 = normalize( lerpResult84_g144 );
				float dotResult9_g144 = dot( normalizeResult7_g144 , float3(0,1,0) );
				float temp_output_12_0_g144 = acos( dotResult9_g144 );
				float NaNPrevention21_g144 = step( 0.01 , abs( ( temp_output_12_0_g144 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g144 = lerp( float3(1,0,0) , appendResult24_g144 , NaNPrevention21_g144);
				float4 transform28_g144 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g144 , 0.0 ));
				float3 normalizeResult49_g144 = normalize( (transform28_g144).xyz );
				float3 RotationAxis30_g144 = normalizeResult49_g144;
				float SlopeCorrectionOffset269 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude268 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g144 = ( saturate( ( (0.0 + ((StaticNoise202).x - 0.0) * (SlopeCorrectionOffset269 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude268 ) ) * temp_output_12_0_g144 );
				float3 rotatedValue35_g144 = RotateAroundAxis( LocalPivot154, ( v.vertex.xyz + MainBending89_g144 ), RotationAxis30_g144, RotationAngle29_g144 );
				float3 lerpResult52_g144 = lerp( MainBending89_g144 , ( rotatedValue35_g144 - v.vertex.xyz ) , NaNPrevention21_g144);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch282 = lerpResult52_g144;
				#else
				float3 staticSwitch282 = temp_output_265_0;
				#endif
				float3 LocalVertexOffset285 = staticSwitch282;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset285;
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

				float2 uv_MainTex253 = IN.ase_texcoord2.xy;
				float4 tex2DNode253 = tex2D( _MainTex, uv_MainTex253 );
				float2 uv_StemTexture260 = IN.ase_texcoord2.xy;
				float4 tex2DNode260 = tex2D( _StemTexture, uv_StemTexture260 );
				float TextureMask250 = step( 1.5 , IN.ase_texcoord2.xy.x );
				float4 lerpResult267 = lerp( tex2DNode253 , tex2DNode260 , TextureMask250);
				float4 TextureColor272 = lerpResult267;
				float DistanceToCenter177 = distance( float2( 0.5,0.5 ) , IN.ase_texcoord2.xy );
				float ColorBlendStart173 = _ColorBlendStart;
				float ColorBlendEnd190 = _ColorBlendEnd;
				float4 lerpResult255 = lerp( _FlowerColor1 , _FlowerColor2 , ( saturate( ( ( DistanceToCenter177 - ColorBlendStart173 ) / ColorBlendEnd190 ) ) * step( ColorBlendStart173 , DistanceToCenter177 ) ));
				float4 lerpResult264 = lerp( lerpResult255 , _StemColor , TextureMask250);
				float4 Color274 = lerpResult264;
				float4 Albedo284 = ( TextureColor272 * Color274 );
				
				float lerpResult280 = lerp( tex2DNode253.a , tex2DNode260.a , TextureMask250);
				float Opacity283 = lerpResult280;
				
				
				float3 Albedo = Albedo284.rgb;
				float Alpha = Opacity283;
				float AlphaClipThreshold = _AlphaCutoff;

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
	CustomEditor "StylisedFlowerWithStem_MaterialInspector"
	
	
}
/*ASEBEGIN
Version=18600
0;72.57143;1496;810;4618.007;1013.637;4.808033;True;False
Node;AmplifyShaderEditor.CommentaryNode;151;-1287.664,899.8252;Inherit;False;775.0664;638.0813;;10;154;250;241;225;234;194;169;153;152;291;Vertex Colors and UVs Baked Data;1,1,1,1;0;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;152;-1258.882,1372.079;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;156;770.3428,-2303.735;Inherit;False;1907.837;1399.909;;32;284;283;281;280;278;277;274;272;267;264;262;261;260;259;255;253;251;249;247;244;239;235;232;231;219;217;199;197;177;165;161;159;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.DynamicAppendNode;153;-1002.553,1379.543;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector2Node;159;837.4587,-2227.985;Inherit;False;Constant;_Vector33;Vector 33;20;0;Create;True;0;0;False;0;False;0.5,0.5;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.NegateNode;291;-861.774,1378.618;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;161;819.3827,-2099.573;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.CommentaryNode;160;-3709.552,-642.1756;Inherit;False;1528.371;1283.004;;37;185;211;252;257;178;170;172;180;186;167;168;171;183;188;184;198;182;191;166;163;269;268;218;215;209;208;207;206;205;204;193;192;190;189;187;181;173;Material Properties;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;164;-1918.332,-891.875;Inherit;False;1402.995;635.2527;;8;157;158;216;202;196;176;174;175;World Space Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;154;-727.5531,1373.543;Inherit;False;LocalPivot;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;163;-3659.829,-588.5316;Inherit;False;Property;_ColorBlendStart;Color Blend Start;2;0;Create;True;0;0;False;0;False;0.1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.DistanceOpNode;165;1080.561,-2164.288;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;173;-3139.55,-589.3907;Inherit;False;ColorBlendStart;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;166;-3656.732,-487.4366;Inherit;False;Property;_ColorBlendEnd;Color Blend End;3;0;Create;True;0;0;False;0;False;0.15;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;177;1264.872,-2170.484;Inherit;False;DistanceToCenter;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;157;-1832.84,-851.1949;Inherit;False;154;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.VertexColorNode;169;-1247.87,962.394;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;197;957.923,-1928.274;Inherit;False;177;DistanceToCenter;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;170;-2883.542,-361.7005;Float;False;Property;_DBHorizontalPhase;DB Horizontal Phase;21;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;176;-1587.583,-761.397;Inherit;True;Property;_NoiseTexture;Noise Texture;26;1;[NoScaleOffset];Create;True;0;0;False;0;False;f55a7ba0496960f42a83ee7d64320fac;512fa11ad89d84543ad8d6c8d9cb6743;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RegisterLocalVarNode;190;-3137.632,-488.1176;Inherit;False;ColorBlendEnd;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;175;-1565.223,-388.1399;Float;False;Property;_NoisePannerSpeed;Noise Panner Speed;28;0;Create;True;0;0;False;0;False;0.05,0.03;0.08,0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.FunctionNode;158;-1594.552,-845.6038;Inherit;False;WorldSpaceUVs - NHP;-1;;134;88a2e8a391a04e241878bdb87d9283a3;0;1;6;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector4Node;174;-1649.223,-560.1409;Inherit;False;Property;_NoiseTextureTilling;Noise Tilling - Static (XY), Animated (ZW);27;0;Create;False;0;0;False;0;False;1,1,1,1;1,1,1,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;178;-2885.542,-265.7005;Float;False;Property;_DBHorizontalMaxRadius;DB Horizontal Max Radius;22;0;Create;True;0;0;False;0;False;0.05;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;172;-2885.542,-456.7006;Float;False;Property;_DBHorizontalFrequency;DB Horizontal Frequency;20;0;Create;True;0;0;False;0;False;1.16;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;167;-3662.362,358.9204;Inherit;False;Property;_MBWindDirBlend;MB Wind Dir Blend;16;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;168;-3660.362,270.9203;Inherit;False;Global;MBGlobalWindDir;MB Global Wind Dir;28;1;[HideInInspector];Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;199;966.923,-1849.274;Inherit;False;173;ColorBlendStart;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;180;-2884.542,-554.7006;Float;False;Property;_DBHorizontalAmplitude;DB Horizontal Amplitude;19;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;171;-3663.456,180.4723;Float;False;Property;_MBWindDir;MB Wind Dir;14;0;Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;181;-3348.889,250.0605;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;192;-2544.543,-269.7005;Inherit;False;DB_HorizontalMaxRadius;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;183;-3663.456,84.47235;Float;False;Property;_MBPhase;MB Phase;13;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;188;-3664.758,-12.26251;Inherit;False;Property;_MBFrequencyOffset;MB Frequency Offset;12;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;189;-2528.543,-554.7006;Float;False;DB_HorizontalAmplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;196;-1268.859,-631.2019;Inherit;False;WorldSpaceNoise - NHP;-1;;135;af5fa9ff24e18344ebcc05b64d296c57;0;4;22;FLOAT2;0,0;False;20;SAMPLER2D;;False;24;FLOAT4;1,1,1,1;False;19;FLOAT2;0.1,0.1;False;2;COLOR;0;COLOR;16
Node;AmplifyShaderEditor.RegisterLocalVarNode;187;-2498.543,-360.7005;Inherit;False;DB_HorizontalPhase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;184;-3666.854,-106.3965;Float;False;Property;_MBFrequency;MB Frequency;11;0;Create;True;0;0;False;0;False;1.11;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;194;-729.2731,1052.696;Float;False;DB_PhaseShift;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;195;-1927.561,-0.6889648;Inherit;False;1409.019;646.176;;10;248;242;237;221;214;213;212;210;203;201;Detail Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;193;-2526.543,-454.7005;Float;False;DB_HorizontalFrequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;219;1213.924,-1880.274;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;182;-3666.854,-298.3965;Float;False;Property;_MBAmplitude;MB Amplitude;9;0;Create;True;0;0;False;0;False;1.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;185;-3651.591,548.9056;Inherit;False;Property;_MBMaxHeight;MB Max Height;17;0;Create;True;0;0;False;0;False;1;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;198;-3666.854,-202.3965;Float;False;Property;_MBAmplitudeOffset;MB Amplitude Offset;10;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;191;-3666.854,-393.3957;Float;False;Property;_MBDefaultBending;MB Default Bending;8;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;217;1160.025,-1771.953;Inherit;False;190;ColorBlendEnd;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;186;-3658.745,453.6646;Float;False;Property;_MBWindDirOffset;MB Wind Dir Offset;15;0;Create;True;0;0;False;0;False;20;0;0;180;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;216;-734.7421,-558.9988;Inherit;False;AnimatedNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;210;-1864.032,445.3931;Inherit;False;192;DB_HorizontalMaxRadius;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;201;-1862.747,126.7581;Inherit;False;189;DB_HorizontalAmplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;204;-3196.457,452.4725;Inherit;False;MB_WindDirectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;214;-1863.747,201.7581;Inherit;False;193;DB_HorizontalFrequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;203;-1809.205,365.802;Inherit;False;194;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;212;-1785.284,527.9121;Inherit;False;154;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;208;-3144.881,84.47235;Inherit;False;MB_Phase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;200;-251.5986,-638.0399;Inherit;False;2939.909;1278.491;;33;285;282;279;275;276;270;273;265;263;254;256;258;2;3;0;5;4;246;245;243;240;226;230;223;227;229;224;228;220;222;236;233;238;Main Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;207;-3140.279,-298.3965;Float;False;MB_Amplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;211;-3132.295,545.9175;Inherit;False;MB_MaxHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;202;-734.899,-676.604;Inherit;False;StaticNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;234;-1224.665,1224.376;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;218;-3180.279,-202.3965;Inherit;False;MB_AmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;232;1325.924,-1672.273;Inherit;False;173;ColorBlendStart;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;235;1405.924,-1832.274;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;231;1293.924,-1592.273;Inherit;False;177;DistanceToCenter;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;213;-1837.205,280.802;Inherit;False;187;DB_HorizontalPhase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;225;-1178.519,1142.415;Inherit;False;Constant;_Float2;Float 2;26;0;Create;True;0;0;False;0;False;1.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;209;-3182.283,-12.10461;Inherit;False;MB_FrequencyOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;206;-3171.279,-394.3957;Float;False;MB_DefaultBending;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;205;-3174.457,245.9235;Float;False;MB_WindDirection;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;215;-3148.279,-106.3965;Float;False;MB_Frequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;233;-214.6105,-479.5919;Inherit;False;204;MB_WindDirectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;238;-153.8096,367.7421;Inherit;False;194;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;221;-1354.763,110.9111;Float;False;Constant;_Vector2;Vector 2;27;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;228;-150.3437,81.12103;Inherit;False;215;MB_Frequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;220;-132.3437,554.121;Inherit;False;202;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;236;-175.6106,-579.5919;Inherit;False;205;MB_WindDirection;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;222;-156.2717,466.2941;Inherit;False;211;MB_MaxHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;227;-148.3437,-114.879;Inherit;False;207;MB_Amplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;244;1565.924,-1640.273;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;224;-186.2717,174.59;Inherit;False;209;MB_FrequencyOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;223;-156.6106,-384.5919;Inherit;False;216;AnimatedNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;230;-187.3437,-15.87898;Inherit;False;218;MB_AmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;226;-125.3437,275.121;Inherit;False;208;MB_Phase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;237;-1454.824,269.3081;Inherit;False;HorizontalBending - NHP;-1;;136;0b16e2546645f904a949bfd32be36037;0;7;44;FLOAT;1;False;39;FLOAT;1;False;43;FLOAT;1;False;40;FLOAT;0;False;46;FLOAT;2;False;47;FLOAT3;0,0,0;False;45;FLOAT;1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SaturateNode;239;1549.924,-1832.274;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;241;-991.8041,1182.579;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;229;-179.6676,-213.879;Inherit;False;206;MB_DefaultBending;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;249;1741.923,-1736.273;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;247;1699.026,-2002.022;Inherit;False;Property;_FlowerColor2;Flower Color 2;1;0;Create;True;0;0;False;0;False;0.8980392,0.9529412,1,1;0.009433985,0.4014694,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.ColorNode;251;1690.225,-2181.238;Inherit;False;Property;_FlowerColor1;Flower Color 1;0;0;Create;True;0;0;False;0;False;0.7843137,0.454902,0.1411765,1;0.5754717,0.3435538,0.1465824,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;243;171.9655,88.38421;Inherit;False;RotationAngle - NHP;-1;;142;87b0b7c0fc8f1424db43b84d20c2e79b;0;9;36;FLOAT;0;False;35;FLOAT;0;False;34;FLOAT;1;False;28;FLOAT;1;False;47;FLOAT;0;False;29;FLOAT;1;False;46;FLOAT;0;False;42;FLOAT;0.1;False;27;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;242;-1125.49,189.8591;Float;False;Property;_EnableHorizontalBending;Enable Horizontal Bending;18;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;250;-731.8302,1179.196;Float;False;TextureMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;240;167.1175,-494.078;Inherit;False;RotationAxis - NHP;-1;;141;b90648f17dcc4bc449d46e8cf04564ff;0;3;20;FLOAT;0;False;19;FLOAT;0;False;18;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;261;1937.498,-1748.176;Float;False;Property;_StemColor;Stem Color;4;0;Create;True;0;0;False;0;False;0.3960784,0.5647059,0.1019608,1;0.8301887,0.5461104,0.3015307,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;246;610.3895,-498.5919;Inherit;False;MB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;260;815.3457,-1104.697;Inherit;True;Property;_StemTexture;Stem Texture;6;1;[NoScaleOffset];Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.SamplerNode;253;809.4907,-1406.275;Inherit;True;Property;_MainTex;Flower Texture;5;1;[NoScaleOffset];Create;False;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;248;-760.0511,191.2861;Float;False;DB_VertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;245;616.2504,85.51019;Float;False;MB_RotationAngle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;257;-2883.587,-154.0486;Inherit;False;Property;_SlopeCorrectionMagnitude;Slope Correction Magnitude;24;0;Create;True;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;252;-2886.941,-54.81773;Inherit;False;Property;_SlopeCorrectionOffset;Slope Correction Offset;25;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;255;2005.747,-2003.551;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;259;1956.925,-1557.882;Inherit;False;250;TextureMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;262;912.8847,-1202.3;Inherit;False;250;TextureMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;263;889.2224,-342.8931;Inherit;False;246;MB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;267;1265.885,-1339.3;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;254;925.6454,-159.964;Inherit;False;154;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;258;878.2224,-238.8929;Inherit;False;245;MB_RotationAngle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;269;-2515.094,-55.80873;Inherit;False;SlopeCorrectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;264;2271.69,-1795.789;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;256;885.2123,-65.45984;Inherit;False;248;DB_VertexOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;268;-2543.765,-154.2486;Inherit;False;SlopeCorrectionMagnitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;273;1313.543,30.2462;Inherit;False;269;SlopeCorrectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;275;1375.543,127.2462;Inherit;False;202;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;274;2464.233,-1802.258;Inherit;False;Color;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;272;1474.801,-1342.797;Inherit;False;TextureColor;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;270;1386.453,221.1479;Inherit;False;154;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;276;1283.959,-59.62979;Inherit;False;268;SlopeCorrectionMagnitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;265;1188.758,-217.4599;Inherit;False;MainBending - NHP;-1;;143;01dba1f3bc33e4b4fa301d2180819576;0;4;55;FLOAT3;0,0,0;False;53;FLOAT;0;False;59;FLOAT3;0,0,0;False;58;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;279;1652.059,-8.337809;Inherit;False;SlopeCorrection - NHP;-1;;144;af38de3ca0adf3c4ba9b6a3dd482959e;0;5;87;FLOAT3;0,0,0;False;42;FLOAT;1;False;92;FLOAT;0;False;93;FLOAT4;0,0,0,0;False;41;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;277;1971.287,-1235.578;Inherit;False;272;TextureColor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;278;1998.529,-1137.053;Inherit;False;274;Color;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;280;1254.885,-1097.3;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;281;2216.907,-1193.387;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;282;2110.565,-223.3759;Float;False;Property;_EnableSlopeCorrection;Enable Slope Correction;23;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;286;3077.092,-1084.603;Inherit;False;762.4954;633.0168;;6;290;288;289;146;1;287;Master Node;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;283;1469.436,-1103.28;Float;False;Opacity;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;285;2421.911,-223.559;Float;False;LocalVertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;284;2381.509,-1197.241;Float;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;287;3279.602,-1029.638;Inherit;False;284;Albedo;1;0;OBJECT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;290;3174.414,-714.8832;Inherit;False;Property;_AlphaCutoff;Alpha Cutoff;7;0;Create;True;0;0;False;0;False;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;288;3234.505,-602.9194;Inherit;False;285;LocalVertexOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;146;3306.635,-925.976;Inherit;False;Constant;_Float3;Float 3;30;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;289;3278.14,-819.8077;Inherit;False;283;Opacity;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;False;0;False;-1;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;3542.734,-913.4128;Float;False;True;-1;2;StylisedFlowerWithStem_MaterialInspector;0;2;Nicrom/NHP/ASE/Vegetation Studio/Stylised Flower With Stem;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;17;False;False;False;False;False;False;False;False;True;0;False;-1;True;2;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;False;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;5;Include;;False;;Native;Pragma;multi_compile GPU_FRUSTUM_ON __;False;;Custom;Pragma;instancing_options procedural:setupScale;False;;Custom;Pragma;instancing_options procedural:setup forwardadd;False;;Custom;Include;VS_indirect.cginc;False;;Custom;;0;0;Standard;36;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;0;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;False;False;False;False;0;False;-1;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;3.060538,-16.26399;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;153;0;152;1
WireConnection;153;2;152;2
WireConnection;291;0;153;0
WireConnection;154;0;291;0
WireConnection;165;0;159;0
WireConnection;165;1;161;0
WireConnection;173;0;163;0
WireConnection;177;0;165;0
WireConnection;190;0;166;0
WireConnection;158;6;157;0
WireConnection;181;0;171;0
WireConnection;181;1;168;0
WireConnection;181;2;167;0
WireConnection;192;0;178;0
WireConnection;189;0;180;0
WireConnection;196;22;158;0
WireConnection;196;20;176;0
WireConnection;196;24;174;0
WireConnection;196;19;175;0
WireConnection;187;0;170;0
WireConnection;194;0;169;4
WireConnection;193;0;172;0
WireConnection;219;0;197;0
WireConnection;219;1;199;0
WireConnection;216;0;196;16
WireConnection;204;0;186;0
WireConnection;208;0;183;0
WireConnection;207;0;182;0
WireConnection;211;0;185;0
WireConnection;202;0;196;0
WireConnection;218;0;198;0
WireConnection;235;0;219;0
WireConnection;235;1;217;0
WireConnection;209;0;188;0
WireConnection;206;0;191;0
WireConnection;205;0;181;0
WireConnection;215;0;184;0
WireConnection;244;0;232;0
WireConnection;244;1;231;0
WireConnection;237;44;201;0
WireConnection;237;39;214;0
WireConnection;237;43;213;0
WireConnection;237;40;203;0
WireConnection;237;46;210;0
WireConnection;237;47;212;0
WireConnection;239;0;235;0
WireConnection;241;0;225;0
WireConnection;241;1;234;1
WireConnection;249;0;239;0
WireConnection;249;1;244;0
WireConnection;243;36;229;0
WireConnection;243;35;227;0
WireConnection;243;34;230;0
WireConnection;243;28;228;0
WireConnection;243;47;224;0
WireConnection;243;29;226;0
WireConnection;243;46;238;0
WireConnection;243;42;222;0
WireConnection;243;27;220;0
WireConnection;242;1;221;0
WireConnection;242;0;237;0
WireConnection;250;0;241;0
WireConnection;240;20;236;0
WireConnection;240;19;233;0
WireConnection;240;18;223;0
WireConnection;246;0;240;0
WireConnection;248;0;242;0
WireConnection;245;0;243;0
WireConnection;255;0;251;0
WireConnection;255;1;247;0
WireConnection;255;2;249;0
WireConnection;267;0;253;0
WireConnection;267;1;260;0
WireConnection;267;2;262;0
WireConnection;269;0;252;0
WireConnection;264;0;255;0
WireConnection;264;1;261;0
WireConnection;264;2;259;0
WireConnection;268;0;257;0
WireConnection;274;0;264;0
WireConnection;272;0;267;0
WireConnection;265;55;263;0
WireConnection;265;53;258;0
WireConnection;265;59;254;0
WireConnection;265;58;256;0
WireConnection;279;87;265;0
WireConnection;279;42;276;0
WireConnection;279;92;273;0
WireConnection;279;93;275;0
WireConnection;279;41;270;0
WireConnection;280;0;253;4
WireConnection;280;1;260;4
WireConnection;280;2;262;0
WireConnection;281;0;277;0
WireConnection;281;1;278;0
WireConnection;282;1;265;0
WireConnection;282;0;279;0
WireConnection;283;0;280;0
WireConnection;285;0;282;0
WireConnection;284;0;281;0
WireConnection;1;0;287;0
WireConnection;1;3;146;0
WireConnection;1;4;146;0
WireConnection;1;6;289;0
WireConnection;1;7;290;0
WireConnection;1;8;288;0
ASEEND*/
//CHKSM=EC0B3B31EFC00CAEA93EBF9E550FFC5B2FF04CC3