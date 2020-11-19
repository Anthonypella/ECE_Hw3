// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Nicrom/NHP/ASE/Stylised Flower Without Stem"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[ASEBegin]_FlowerColor1("Flower Color 1", Color) = (0.7843137,0.454902,0.1411765,1)
		_FlowerColor2("Flower Color 2", Color) = (0.8980392,0.9529412,1,1)
		_ColorBlendStart("Color Blend Start", Range( 0 , 1)) = 0.1
		_ColorBlendEnd("Color Blend End", Range( 0 , 1)) = 0.15
		[NoScaleOffset][Header(Textures)]_MainTex("Flower Texture", 2D) = "white" {}
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
		_MBMaxHeight("MB Max Height", Float) = 0.5
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


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
			float4 _FlowerColor2;
			float4 _NoiseTextureTilling;
			float4 _FlowerColor1;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
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

				float lerpResult180 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection184 = lerpResult180;
				float MB_WindDirectiionOffset186 = _MBWindDirOffset;
				float4 transform1_g109 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord138 = v.texcoord1.xyzw.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult139 = (float3(texCoord138.x , 0.0 , texCoord138.y));
				float3 LocalPivot140 = -appendResult139;
				float4 transform2_g109 = mul(GetObjectToWorldMatrix(),float4( LocalPivot140 , 0.0 ));
				float2 UVs27_g110 = ( (transform1_g109).xz + (transform2_g109).xz );
				float4 temp_output_24_0_g110 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g110 = (temp_output_24_0_g110).zw;
				float2 panner7_g110 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise187 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g110 * AnimatedNoiseTilling29_g110 ) + panner7_g110 ), 0, 0.0) );
				float temp_output_11_0_g115 = radians( ( ( MB_WindDirection184 + ( MB_WindDirectiionOffset186 * (-1.0 + ((AnimatedNoise187).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g115 = (float3(cos( temp_output_11_0_g115 ) , 0.0 , sin( temp_output_11_0_g115 )));
				float4 transform15_g115 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g115 , 0.0 ));
				float3 normalizeResult34_g115 = normalize( (transform15_g115).xyz );
				float3 MB_RotationAxis229 = normalizeResult34_g115;
				float3 RotationAxis56_g116 = MB_RotationAxis229;
				float MB_Amplitude203 = _MBAmplitude;
				float MB_AmplitudeOffset185 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g110 = (temp_output_24_0_g110).xy;
				float4 StaticNoise197 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g110 * StaticNoileTilling28_g110 ), 0, 0.0) );
				float4 StaticWorldNoise31_g114 = StaticNoise197;
				float4 transform8_g114 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency201 = _MBFrequency;
				float MB_FrequencyOffset200 = _MBFrequencyOffset;
				float DB_PhaseShift176 = v.ase_color.a;
				float MB_Phase190 = _MBPhase;
				float MB_DefaultBending195 = _MBDefaultBending;
				float MB_MaxHeight196 = _MBMaxHeight;
				float MB_RotationAngle228 = radians( ( ( ( ( MB_Amplitude203 + ( MB_AmplitudeOffset185 * (StaticWorldNoise31_g114).x ) ) * sin( ( ( ( transform8_g114.x + transform8_g114.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency201 + ( MB_FrequencyOffset200 * (StaticWorldNoise31_g114).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift176 ) ) ) * MB_Phase190 ) ) ) + MB_DefaultBending195 ) * ( v.vertex.xyz.y / MB_MaxHeight196 ) ) );
				float RotationAngle54_g116 = MB_RotationAngle228;
				float3 PivotPoint60_g116 = LocalPivot140;
				float3 break62_g116 = PivotPoint60_g116;
				float3 appendResult45_g116 = (float3(break62_g116.x , v.vertex.xyz.y , break62_g116.z));
				float3 rotatedValue30_g116 = RotateAroundAxis( appendResult45_g116, v.vertex.xyz, RotationAxis56_g116, RotationAngle54_g116 );
				float DB_HorizontalAmplitude178 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency179 = _DBHorizontalFrequency;
				float Frequency41_g111 = DB_HorizontalFrequency179;
				float4 transform5_g111 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase171 = _DBHorizontalPhase;
				float3 PivotPoint49_g111 = LocalPivot140;
				float3 break52_g111 = PivotPoint49_g111;
				float3 appendResult20_g111 = (float3(break52_g111.x , v.vertex.xyz.y , break52_g111.z));
				float DB_HorizontalMaxRadius169 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g111 = RotateAroundAxis( PivotPoint49_g111, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude178 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g111 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift176 ) ) ) + ( ( ( transform5_g111.x + transform5_g111.z ) + ( ( _TimeParameters.x ) * Frequency41_g111 ) ) * DB_HorizontalPhase171 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g111 ) / DB_HorizontalMaxRadius169 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch222 = ( ( rotatedValue33_g111 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch222 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset230 = staticSwitch222;
				float3 rotatedValue34_g116 = RotateAroundAxis( PivotPoint60_g116, ( rotatedValue30_g116 + DB_VertexOffset230 ), RotationAxis56_g116, RotationAngle54_g116 );
				float3 temp_output_244_0 = ( ( rotatedValue34_g116 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g117 = temp_output_244_0;
				float3 appendResult15_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g117 , 0.0 ));
				float4 break20_g117 = transform17_g117;
				float3 appendResult24_g117 = (float3(-break20_g117.z , 0.0 , break20_g117.x));
				float3 appendResult3_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g117 , 0.0 ));
				float3 lerpResult84_g117 = lerp( float3(0,1,0) , (transform4_g117).xyz , step( 1E-06 , ( abs( transform4_g117.x ) + abs( transform4_g117.z ) ) ));
				float3 normalizeResult7_g117 = normalize( lerpResult84_g117 );
				float dotResult9_g117 = dot( normalizeResult7_g117 , float3(0,1,0) );
				float temp_output_12_0_g117 = acos( dotResult9_g117 );
				float NaNPrevention21_g117 = step( 0.01 , abs( ( temp_output_12_0_g117 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g117 = lerp( float3(1,0,0) , appendResult24_g117 , NaNPrevention21_g117);
				float4 transform28_g117 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g117 , 0.0 ));
				float3 normalizeResult49_g117 = normalize( (transform28_g117).xyz );
				float3 RotationAxis30_g117 = normalizeResult49_g117;
				float SlopeCorrectionOffset243 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude240 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g117 = ( saturate( ( (0.0 + ((StaticNoise197).x - 0.0) * (SlopeCorrectionOffset243 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude240 ) ) * temp_output_12_0_g117 );
				float3 rotatedValue35_g117 = RotateAroundAxis( LocalPivot140, ( v.vertex.xyz + MainBending89_g117 ), RotationAxis30_g117, RotationAngle29_g117 );
				float3 lerpResult52_g117 = lerp( MainBending89_g117 , ( rotatedValue35_g117 - v.vertex.xyz ) , NaNPrevention21_g117);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch257 = lerpResult52_g117;
				#else
				float3 staticSwitch257 = temp_output_244_0;
				#endif
				float3 LocalVertexOffset260 = staticSwitch257;
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset260;
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

				float2 uv_MainTex241 = IN.ase_texcoord7.xy;
				float4 tex2DNode241 = tex2D( _MainTex, uv_MainTex241 );
				float4 MainTextureColor251 = tex2DNode241;
				float DistanceToCenter177 = distance( float2( 0.5,0.5 ) , IN.ase_texcoord7.xy );
				float ColorBlendStart166 = _ColorBlendStart;
				float ColorBlendEnd192 = _ColorBlendEnd;
				float4 lerpResult242 = lerp( _FlowerColor1 , _FlowerColor2 , ( saturate( ( ( DistanceToCenter177 - ColorBlendStart166 ) / ColorBlendEnd192 ) ) * step( ColorBlendStart166 , DistanceToCenter177 ) ));
				float4 Color252 = lerpResult242;
				float4 Albedo258 = ( MainTextureColor251 * Color252 );
				
				float Opacity261 = tex2DNode241.a;
				
				float3 Albedo = Albedo258.rgb;
				float3 Normal = float3(0, 0, 1);
				float3 Emission = 0;
				float3 Specular = 0.5;
				float Metallic = 0.0;
				float Smoothness = 0.0;
				float Occlusion = 1;
				float Alpha = Opacity261;
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


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
			float4 _FlowerColor2;
			float4 _NoiseTextureTilling;
			float4 _FlowerColor1;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
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

				float lerpResult180 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection184 = lerpResult180;
				float MB_WindDirectiionOffset186 = _MBWindDirOffset;
				float4 transform1_g109 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord138 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult139 = (float3(texCoord138.x , 0.0 , texCoord138.y));
				float3 LocalPivot140 = -appendResult139;
				float4 transform2_g109 = mul(GetObjectToWorldMatrix(),float4( LocalPivot140 , 0.0 ));
				float2 UVs27_g110 = ( (transform1_g109).xz + (transform2_g109).xz );
				float4 temp_output_24_0_g110 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g110 = (temp_output_24_0_g110).zw;
				float2 panner7_g110 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise187 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g110 * AnimatedNoiseTilling29_g110 ) + panner7_g110 ), 0, 0.0) );
				float temp_output_11_0_g115 = radians( ( ( MB_WindDirection184 + ( MB_WindDirectiionOffset186 * (-1.0 + ((AnimatedNoise187).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g115 = (float3(cos( temp_output_11_0_g115 ) , 0.0 , sin( temp_output_11_0_g115 )));
				float4 transform15_g115 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g115 , 0.0 ));
				float3 normalizeResult34_g115 = normalize( (transform15_g115).xyz );
				float3 MB_RotationAxis229 = normalizeResult34_g115;
				float3 RotationAxis56_g116 = MB_RotationAxis229;
				float MB_Amplitude203 = _MBAmplitude;
				float MB_AmplitudeOffset185 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g110 = (temp_output_24_0_g110).xy;
				float4 StaticNoise197 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g110 * StaticNoileTilling28_g110 ), 0, 0.0) );
				float4 StaticWorldNoise31_g114 = StaticNoise197;
				float4 transform8_g114 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency201 = _MBFrequency;
				float MB_FrequencyOffset200 = _MBFrequencyOffset;
				float DB_PhaseShift176 = v.ase_color.a;
				float MB_Phase190 = _MBPhase;
				float MB_DefaultBending195 = _MBDefaultBending;
				float MB_MaxHeight196 = _MBMaxHeight;
				float MB_RotationAngle228 = radians( ( ( ( ( MB_Amplitude203 + ( MB_AmplitudeOffset185 * (StaticWorldNoise31_g114).x ) ) * sin( ( ( ( transform8_g114.x + transform8_g114.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency201 + ( MB_FrequencyOffset200 * (StaticWorldNoise31_g114).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift176 ) ) ) * MB_Phase190 ) ) ) + MB_DefaultBending195 ) * ( v.vertex.xyz.y / MB_MaxHeight196 ) ) );
				float RotationAngle54_g116 = MB_RotationAngle228;
				float3 PivotPoint60_g116 = LocalPivot140;
				float3 break62_g116 = PivotPoint60_g116;
				float3 appendResult45_g116 = (float3(break62_g116.x , v.vertex.xyz.y , break62_g116.z));
				float3 rotatedValue30_g116 = RotateAroundAxis( appendResult45_g116, v.vertex.xyz, RotationAxis56_g116, RotationAngle54_g116 );
				float DB_HorizontalAmplitude178 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency179 = _DBHorizontalFrequency;
				float Frequency41_g111 = DB_HorizontalFrequency179;
				float4 transform5_g111 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase171 = _DBHorizontalPhase;
				float3 PivotPoint49_g111 = LocalPivot140;
				float3 break52_g111 = PivotPoint49_g111;
				float3 appendResult20_g111 = (float3(break52_g111.x , v.vertex.xyz.y , break52_g111.z));
				float DB_HorizontalMaxRadius169 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g111 = RotateAroundAxis( PivotPoint49_g111, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude178 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g111 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift176 ) ) ) + ( ( ( transform5_g111.x + transform5_g111.z ) + ( ( _TimeParameters.x ) * Frequency41_g111 ) ) * DB_HorizontalPhase171 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g111 ) / DB_HorizontalMaxRadius169 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch222 = ( ( rotatedValue33_g111 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch222 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset230 = staticSwitch222;
				float3 rotatedValue34_g116 = RotateAroundAxis( PivotPoint60_g116, ( rotatedValue30_g116 + DB_VertexOffset230 ), RotationAxis56_g116, RotationAngle54_g116 );
				float3 temp_output_244_0 = ( ( rotatedValue34_g116 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g117 = temp_output_244_0;
				float3 appendResult15_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g117 , 0.0 ));
				float4 break20_g117 = transform17_g117;
				float3 appendResult24_g117 = (float3(-break20_g117.z , 0.0 , break20_g117.x));
				float3 appendResult3_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g117 , 0.0 ));
				float3 lerpResult84_g117 = lerp( float3(0,1,0) , (transform4_g117).xyz , step( 1E-06 , ( abs( transform4_g117.x ) + abs( transform4_g117.z ) ) ));
				float3 normalizeResult7_g117 = normalize( lerpResult84_g117 );
				float dotResult9_g117 = dot( normalizeResult7_g117 , float3(0,1,0) );
				float temp_output_12_0_g117 = acos( dotResult9_g117 );
				float NaNPrevention21_g117 = step( 0.01 , abs( ( temp_output_12_0_g117 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g117 = lerp( float3(1,0,0) , appendResult24_g117 , NaNPrevention21_g117);
				float4 transform28_g117 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g117 , 0.0 ));
				float3 normalizeResult49_g117 = normalize( (transform28_g117).xyz );
				float3 RotationAxis30_g117 = normalizeResult49_g117;
				float SlopeCorrectionOffset243 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude240 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g117 = ( saturate( ( (0.0 + ((StaticNoise197).x - 0.0) * (SlopeCorrectionOffset243 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude240 ) ) * temp_output_12_0_g117 );
				float3 rotatedValue35_g117 = RotateAroundAxis( LocalPivot140, ( v.vertex.xyz + MainBending89_g117 ), RotationAxis30_g117, RotationAngle29_g117 );
				float3 lerpResult52_g117 = lerp( MainBending89_g117 , ( rotatedValue35_g117 - v.vertex.xyz ) , NaNPrevention21_g117);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch257 = lerpResult52_g117;
				#else
				float3 staticSwitch257 = temp_output_244_0;
				#endif
				float3 LocalVertexOffset260 = staticSwitch257;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset260;
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

				float2 uv_MainTex241 = IN.ase_texcoord2.xy;
				float4 tex2DNode241 = tex2D( _MainTex, uv_MainTex241 );
				float Opacity261 = tex2DNode241.a;
				
				float Alpha = Opacity261;
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


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
			float4 _FlowerColor2;
			float4 _NoiseTextureTilling;
			float4 _FlowerColor1;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
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

				float lerpResult180 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection184 = lerpResult180;
				float MB_WindDirectiionOffset186 = _MBWindDirOffset;
				float4 transform1_g109 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord138 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult139 = (float3(texCoord138.x , 0.0 , texCoord138.y));
				float3 LocalPivot140 = -appendResult139;
				float4 transform2_g109 = mul(GetObjectToWorldMatrix(),float4( LocalPivot140 , 0.0 ));
				float2 UVs27_g110 = ( (transform1_g109).xz + (transform2_g109).xz );
				float4 temp_output_24_0_g110 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g110 = (temp_output_24_0_g110).zw;
				float2 panner7_g110 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise187 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g110 * AnimatedNoiseTilling29_g110 ) + panner7_g110 ), 0, 0.0) );
				float temp_output_11_0_g115 = radians( ( ( MB_WindDirection184 + ( MB_WindDirectiionOffset186 * (-1.0 + ((AnimatedNoise187).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g115 = (float3(cos( temp_output_11_0_g115 ) , 0.0 , sin( temp_output_11_0_g115 )));
				float4 transform15_g115 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g115 , 0.0 ));
				float3 normalizeResult34_g115 = normalize( (transform15_g115).xyz );
				float3 MB_RotationAxis229 = normalizeResult34_g115;
				float3 RotationAxis56_g116 = MB_RotationAxis229;
				float MB_Amplitude203 = _MBAmplitude;
				float MB_AmplitudeOffset185 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g110 = (temp_output_24_0_g110).xy;
				float4 StaticNoise197 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g110 * StaticNoileTilling28_g110 ), 0, 0.0) );
				float4 StaticWorldNoise31_g114 = StaticNoise197;
				float4 transform8_g114 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency201 = _MBFrequency;
				float MB_FrequencyOffset200 = _MBFrequencyOffset;
				float DB_PhaseShift176 = v.ase_color.a;
				float MB_Phase190 = _MBPhase;
				float MB_DefaultBending195 = _MBDefaultBending;
				float MB_MaxHeight196 = _MBMaxHeight;
				float MB_RotationAngle228 = radians( ( ( ( ( MB_Amplitude203 + ( MB_AmplitudeOffset185 * (StaticWorldNoise31_g114).x ) ) * sin( ( ( ( transform8_g114.x + transform8_g114.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency201 + ( MB_FrequencyOffset200 * (StaticWorldNoise31_g114).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift176 ) ) ) * MB_Phase190 ) ) ) + MB_DefaultBending195 ) * ( v.vertex.xyz.y / MB_MaxHeight196 ) ) );
				float RotationAngle54_g116 = MB_RotationAngle228;
				float3 PivotPoint60_g116 = LocalPivot140;
				float3 break62_g116 = PivotPoint60_g116;
				float3 appendResult45_g116 = (float3(break62_g116.x , v.vertex.xyz.y , break62_g116.z));
				float3 rotatedValue30_g116 = RotateAroundAxis( appendResult45_g116, v.vertex.xyz, RotationAxis56_g116, RotationAngle54_g116 );
				float DB_HorizontalAmplitude178 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency179 = _DBHorizontalFrequency;
				float Frequency41_g111 = DB_HorizontalFrequency179;
				float4 transform5_g111 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase171 = _DBHorizontalPhase;
				float3 PivotPoint49_g111 = LocalPivot140;
				float3 break52_g111 = PivotPoint49_g111;
				float3 appendResult20_g111 = (float3(break52_g111.x , v.vertex.xyz.y , break52_g111.z));
				float DB_HorizontalMaxRadius169 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g111 = RotateAroundAxis( PivotPoint49_g111, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude178 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g111 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift176 ) ) ) + ( ( ( transform5_g111.x + transform5_g111.z ) + ( ( _TimeParameters.x ) * Frequency41_g111 ) ) * DB_HorizontalPhase171 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g111 ) / DB_HorizontalMaxRadius169 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch222 = ( ( rotatedValue33_g111 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch222 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset230 = staticSwitch222;
				float3 rotatedValue34_g116 = RotateAroundAxis( PivotPoint60_g116, ( rotatedValue30_g116 + DB_VertexOffset230 ), RotationAxis56_g116, RotationAngle54_g116 );
				float3 temp_output_244_0 = ( ( rotatedValue34_g116 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g117 = temp_output_244_0;
				float3 appendResult15_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g117 , 0.0 ));
				float4 break20_g117 = transform17_g117;
				float3 appendResult24_g117 = (float3(-break20_g117.z , 0.0 , break20_g117.x));
				float3 appendResult3_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g117 , 0.0 ));
				float3 lerpResult84_g117 = lerp( float3(0,1,0) , (transform4_g117).xyz , step( 1E-06 , ( abs( transform4_g117.x ) + abs( transform4_g117.z ) ) ));
				float3 normalizeResult7_g117 = normalize( lerpResult84_g117 );
				float dotResult9_g117 = dot( normalizeResult7_g117 , float3(0,1,0) );
				float temp_output_12_0_g117 = acos( dotResult9_g117 );
				float NaNPrevention21_g117 = step( 0.01 , abs( ( temp_output_12_0_g117 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g117 = lerp( float3(1,0,0) , appendResult24_g117 , NaNPrevention21_g117);
				float4 transform28_g117 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g117 , 0.0 ));
				float3 normalizeResult49_g117 = normalize( (transform28_g117).xyz );
				float3 RotationAxis30_g117 = normalizeResult49_g117;
				float SlopeCorrectionOffset243 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude240 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g117 = ( saturate( ( (0.0 + ((StaticNoise197).x - 0.0) * (SlopeCorrectionOffset243 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude240 ) ) * temp_output_12_0_g117 );
				float3 rotatedValue35_g117 = RotateAroundAxis( LocalPivot140, ( v.vertex.xyz + MainBending89_g117 ), RotationAxis30_g117, RotationAngle29_g117 );
				float3 lerpResult52_g117 = lerp( MainBending89_g117 , ( rotatedValue35_g117 - v.vertex.xyz ) , NaNPrevention21_g117);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch257 = lerpResult52_g117;
				#else
				float3 staticSwitch257 = temp_output_244_0;
				#endif
				float3 LocalVertexOffset260 = staticSwitch257;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset260;
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

				float2 uv_MainTex241 = IN.ase_texcoord2.xy;
				float4 tex2DNode241 = tex2D( _MainTex, uv_MainTex241 );
				float Opacity261 = tex2DNode241.a;
				
				float Alpha = Opacity261;
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
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


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
			float4 _FlowerColor2;
			float4 _NoiseTextureTilling;
			float4 _FlowerColor1;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
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

				float lerpResult180 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection184 = lerpResult180;
				float MB_WindDirectiionOffset186 = _MBWindDirOffset;
				float4 transform1_g109 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord138 = v.texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult139 = (float3(texCoord138.x , 0.0 , texCoord138.y));
				float3 LocalPivot140 = -appendResult139;
				float4 transform2_g109 = mul(GetObjectToWorldMatrix(),float4( LocalPivot140 , 0.0 ));
				float2 UVs27_g110 = ( (transform1_g109).xz + (transform2_g109).xz );
				float4 temp_output_24_0_g110 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g110 = (temp_output_24_0_g110).zw;
				float2 panner7_g110 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise187 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g110 * AnimatedNoiseTilling29_g110 ) + panner7_g110 ), 0, 0.0) );
				float temp_output_11_0_g115 = radians( ( ( MB_WindDirection184 + ( MB_WindDirectiionOffset186 * (-1.0 + ((AnimatedNoise187).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g115 = (float3(cos( temp_output_11_0_g115 ) , 0.0 , sin( temp_output_11_0_g115 )));
				float4 transform15_g115 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g115 , 0.0 ));
				float3 normalizeResult34_g115 = normalize( (transform15_g115).xyz );
				float3 MB_RotationAxis229 = normalizeResult34_g115;
				float3 RotationAxis56_g116 = MB_RotationAxis229;
				float MB_Amplitude203 = _MBAmplitude;
				float MB_AmplitudeOffset185 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g110 = (temp_output_24_0_g110).xy;
				float4 StaticNoise197 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g110 * StaticNoileTilling28_g110 ), 0, 0.0) );
				float4 StaticWorldNoise31_g114 = StaticNoise197;
				float4 transform8_g114 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency201 = _MBFrequency;
				float MB_FrequencyOffset200 = _MBFrequencyOffset;
				float DB_PhaseShift176 = v.ase_color.a;
				float MB_Phase190 = _MBPhase;
				float MB_DefaultBending195 = _MBDefaultBending;
				float MB_MaxHeight196 = _MBMaxHeight;
				float MB_RotationAngle228 = radians( ( ( ( ( MB_Amplitude203 + ( MB_AmplitudeOffset185 * (StaticWorldNoise31_g114).x ) ) * sin( ( ( ( transform8_g114.x + transform8_g114.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency201 + ( MB_FrequencyOffset200 * (StaticWorldNoise31_g114).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift176 ) ) ) * MB_Phase190 ) ) ) + MB_DefaultBending195 ) * ( v.vertex.xyz.y / MB_MaxHeight196 ) ) );
				float RotationAngle54_g116 = MB_RotationAngle228;
				float3 PivotPoint60_g116 = LocalPivot140;
				float3 break62_g116 = PivotPoint60_g116;
				float3 appendResult45_g116 = (float3(break62_g116.x , v.vertex.xyz.y , break62_g116.z));
				float3 rotatedValue30_g116 = RotateAroundAxis( appendResult45_g116, v.vertex.xyz, RotationAxis56_g116, RotationAngle54_g116 );
				float DB_HorizontalAmplitude178 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency179 = _DBHorizontalFrequency;
				float Frequency41_g111 = DB_HorizontalFrequency179;
				float4 transform5_g111 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase171 = _DBHorizontalPhase;
				float3 PivotPoint49_g111 = LocalPivot140;
				float3 break52_g111 = PivotPoint49_g111;
				float3 appendResult20_g111 = (float3(break52_g111.x , v.vertex.xyz.y , break52_g111.z));
				float DB_HorizontalMaxRadius169 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g111 = RotateAroundAxis( PivotPoint49_g111, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude178 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g111 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift176 ) ) ) + ( ( ( transform5_g111.x + transform5_g111.z ) + ( ( _TimeParameters.x ) * Frequency41_g111 ) ) * DB_HorizontalPhase171 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g111 ) / DB_HorizontalMaxRadius169 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch222 = ( ( rotatedValue33_g111 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch222 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset230 = staticSwitch222;
				float3 rotatedValue34_g116 = RotateAroundAxis( PivotPoint60_g116, ( rotatedValue30_g116 + DB_VertexOffset230 ), RotationAxis56_g116, RotationAngle54_g116 );
				float3 temp_output_244_0 = ( ( rotatedValue34_g116 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g117 = temp_output_244_0;
				float3 appendResult15_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g117 , 0.0 ));
				float4 break20_g117 = transform17_g117;
				float3 appendResult24_g117 = (float3(-break20_g117.z , 0.0 , break20_g117.x));
				float3 appendResult3_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g117 , 0.0 ));
				float3 lerpResult84_g117 = lerp( float3(0,1,0) , (transform4_g117).xyz , step( 1E-06 , ( abs( transform4_g117.x ) + abs( transform4_g117.z ) ) ));
				float3 normalizeResult7_g117 = normalize( lerpResult84_g117 );
				float dotResult9_g117 = dot( normalizeResult7_g117 , float3(0,1,0) );
				float temp_output_12_0_g117 = acos( dotResult9_g117 );
				float NaNPrevention21_g117 = step( 0.01 , abs( ( temp_output_12_0_g117 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g117 = lerp( float3(1,0,0) , appendResult24_g117 , NaNPrevention21_g117);
				float4 transform28_g117 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g117 , 0.0 ));
				float3 normalizeResult49_g117 = normalize( (transform28_g117).xyz );
				float3 RotationAxis30_g117 = normalizeResult49_g117;
				float SlopeCorrectionOffset243 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude240 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g117 = ( saturate( ( (0.0 + ((StaticNoise197).x - 0.0) * (SlopeCorrectionOffset243 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude240 ) ) * temp_output_12_0_g117 );
				float3 rotatedValue35_g117 = RotateAroundAxis( LocalPivot140, ( v.vertex.xyz + MainBending89_g117 ), RotationAxis30_g117, RotationAngle29_g117 );
				float3 lerpResult52_g117 = lerp( MainBending89_g117 , ( rotatedValue35_g117 - v.vertex.xyz ) , NaNPrevention21_g117);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch257 = lerpResult52_g117;
				#else
				float3 staticSwitch257 = temp_output_244_0;
				#endif
				float3 LocalVertexOffset260 = staticSwitch257;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset260;
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

				float2 uv_MainTex241 = IN.ase_texcoord2.xy;
				float4 tex2DNode241 = tex2D( _MainTex, uv_MainTex241 );
				float4 MainTextureColor251 = tex2DNode241;
				float DistanceToCenter177 = distance( float2( 0.5,0.5 ) , IN.ase_texcoord2.xy );
				float ColorBlendStart166 = _ColorBlendStart;
				float ColorBlendEnd192 = _ColorBlendEnd;
				float4 lerpResult242 = lerp( _FlowerColor1 , _FlowerColor2 , ( saturate( ( ( DistanceToCenter177 - ColorBlendStart166 ) / ColorBlendEnd192 ) ) * step( ColorBlendStart166 , DistanceToCenter177 ) ));
				float4 Color252 = lerpResult242;
				float4 Albedo258 = ( MainTextureColor251 * Color252 );
				
				float Opacity261 = tex2DNode241.a;
				
				
				float3 Albedo = Albedo258.rgb;
				float3 Emission = 0;
				float Alpha = Opacity261;
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
			ColorMask RGBA

			HLSLPROGRAM
			#define _NORMAL_DROPOFF_TS 1
			#pragma multi_compile_instancing
			#pragma multi_compile _ LOD_FADE_CROSSFADE
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
			float4 _FlowerColor2;
			float4 _NoiseTextureTilling;
			float4 _FlowerColor1;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _ColorBlendStart;
			float _SlopeCorrectionMagnitude;
			float _SlopeCorrectionOffset;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
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
				UNITY_SETUP_INSTANCE_ID( v );
				UNITY_TRANSFER_INSTANCE_ID( v, o );
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO( o );

				float lerpResult180 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection184 = lerpResult180;
				float MB_WindDirectiionOffset186 = _MBWindDirOffset;
				float4 transform1_g109 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 texCoord138 = v.ase_texcoord1.xy * float2( 1,1 ) + float2( 0,0 );
				float3 appendResult139 = (float3(texCoord138.x , 0.0 , texCoord138.y));
				float3 LocalPivot140 = -appendResult139;
				float4 transform2_g109 = mul(GetObjectToWorldMatrix(),float4( LocalPivot140 , 0.0 ));
				float2 UVs27_g110 = ( (transform1_g109).xz + (transform2_g109).xz );
				float4 temp_output_24_0_g110 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g110 = (temp_output_24_0_g110).zw;
				float2 panner7_g110 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise187 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g110 * AnimatedNoiseTilling29_g110 ) + panner7_g110 ), 0, 0.0) );
				float temp_output_11_0_g115 = radians( ( ( MB_WindDirection184 + ( MB_WindDirectiionOffset186 * (-1.0 + ((AnimatedNoise187).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g115 = (float3(cos( temp_output_11_0_g115 ) , 0.0 , sin( temp_output_11_0_g115 )));
				float4 transform15_g115 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g115 , 0.0 ));
				float3 normalizeResult34_g115 = normalize( (transform15_g115).xyz );
				float3 MB_RotationAxis229 = normalizeResult34_g115;
				float3 RotationAxis56_g116 = MB_RotationAxis229;
				float MB_Amplitude203 = _MBAmplitude;
				float MB_AmplitudeOffset185 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g110 = (temp_output_24_0_g110).xy;
				float4 StaticNoise197 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g110 * StaticNoileTilling28_g110 ), 0, 0.0) );
				float4 StaticWorldNoise31_g114 = StaticNoise197;
				float4 transform8_g114 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency201 = _MBFrequency;
				float MB_FrequencyOffset200 = _MBFrequencyOffset;
				float DB_PhaseShift176 = v.ase_color.a;
				float MB_Phase190 = _MBPhase;
				float MB_DefaultBending195 = _MBDefaultBending;
				float MB_MaxHeight196 = _MBMaxHeight;
				float MB_RotationAngle228 = radians( ( ( ( ( MB_Amplitude203 + ( MB_AmplitudeOffset185 * (StaticWorldNoise31_g114).x ) ) * sin( ( ( ( transform8_g114.x + transform8_g114.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency201 + ( MB_FrequencyOffset200 * (StaticWorldNoise31_g114).x ) ) ) + ( ( 2.0 * PI ) * DB_PhaseShift176 ) ) ) * MB_Phase190 ) ) ) + MB_DefaultBending195 ) * ( v.vertex.xyz.y / MB_MaxHeight196 ) ) );
				float RotationAngle54_g116 = MB_RotationAngle228;
				float3 PivotPoint60_g116 = LocalPivot140;
				float3 break62_g116 = PivotPoint60_g116;
				float3 appendResult45_g116 = (float3(break62_g116.x , v.vertex.xyz.y , break62_g116.z));
				float3 rotatedValue30_g116 = RotateAroundAxis( appendResult45_g116, v.vertex.xyz, RotationAxis56_g116, RotationAngle54_g116 );
				float DB_HorizontalAmplitude178 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency179 = _DBHorizontalFrequency;
				float Frequency41_g111 = DB_HorizontalFrequency179;
				float4 transform5_g111 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase171 = _DBHorizontalPhase;
				float3 PivotPoint49_g111 = LocalPivot140;
				float3 break52_g111 = PivotPoint49_g111;
				float3 appendResult20_g111 = (float3(break52_g111.x , v.vertex.xyz.y , break52_g111.z));
				float DB_HorizontalMaxRadius169 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g111 = RotateAroundAxis( PivotPoint49_g111, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude178 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g111 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift176 ) ) ) + ( ( ( transform5_g111.x + transform5_g111.z ) + ( ( _TimeParameters.x ) * Frequency41_g111 ) ) * DB_HorizontalPhase171 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g111 ) / DB_HorizontalMaxRadius169 ) ) ) );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch222 = ( ( rotatedValue33_g111 - v.vertex.xyz ) * 1.0 );
				#else
				float3 staticSwitch222 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset230 = staticSwitch222;
				float3 rotatedValue34_g116 = RotateAroundAxis( PivotPoint60_g116, ( rotatedValue30_g116 + DB_VertexOffset230 ), RotationAxis56_g116, RotationAngle54_g116 );
				float3 temp_output_244_0 = ( ( rotatedValue34_g116 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				float3 MainBending89_g117 = temp_output_244_0;
				float3 appendResult15_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform17_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult15_g117 , 0.0 ));
				float4 break20_g117 = transform17_g117;
				float3 appendResult24_g117 = (float3(-break20_g117.z , 0.0 , break20_g117.x));
				float3 appendResult3_g117 = (float3(0.0 , 1.0 , 0.0));
				float4 transform4_g117 = mul(GetObjectToWorldMatrix(),float4( appendResult3_g117 , 0.0 ));
				float3 lerpResult84_g117 = lerp( float3(0,1,0) , (transform4_g117).xyz , step( 1E-06 , ( abs( transform4_g117.x ) + abs( transform4_g117.z ) ) ));
				float3 normalizeResult7_g117 = normalize( lerpResult84_g117 );
				float dotResult9_g117 = dot( normalizeResult7_g117 , float3(0,1,0) );
				float temp_output_12_0_g117 = acos( dotResult9_g117 );
				float NaNPrevention21_g117 = step( 0.01 , abs( ( temp_output_12_0_g117 * ( 180.0 / PI ) ) ) );
				float3 lerpResult26_g117 = lerp( float3(1,0,0) , appendResult24_g117 , NaNPrevention21_g117);
				float4 transform28_g117 = mul(GetWorldToObjectMatrix(),float4( lerpResult26_g117 , 0.0 ));
				float3 normalizeResult49_g117 = normalize( (transform28_g117).xyz );
				float3 RotationAxis30_g117 = normalizeResult49_g117;
				float SlopeCorrectionOffset243 = _SlopeCorrectionOffset;
				float SlopeCorrectionMagnitude240 = _SlopeCorrectionMagnitude;
				float RotationAngle29_g117 = ( saturate( ( (0.0 + ((StaticNoise197).x - 0.0) * (SlopeCorrectionOffset243 - 0.0) / (1.0 - 0.0)) + SlopeCorrectionMagnitude240 ) ) * temp_output_12_0_g117 );
				float3 rotatedValue35_g117 = RotateAroundAxis( LocalPivot140, ( v.vertex.xyz + MainBending89_g117 ), RotationAxis30_g117, RotationAngle29_g117 );
				float3 lerpResult52_g117 = lerp( MainBending89_g117 , ( rotatedValue35_g117 - v.vertex.xyz ) , NaNPrevention21_g117);
				#ifdef _ENABLESLOPECORRECTION_ON
				float3 staticSwitch257 = lerpResult52_g117;
				#else
				float3 staticSwitch257 = temp_output_244_0;
				#endif
				float3 LocalVertexOffset260 = staticSwitch257;
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset260;
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

				float2 uv_MainTex241 = IN.ase_texcoord2.xy;
				float4 tex2DNode241 = tex2D( _MainTex, uv_MainTex241 );
				float4 MainTextureColor251 = tex2DNode241;
				float DistanceToCenter177 = distance( float2( 0.5,0.5 ) , IN.ase_texcoord2.xy );
				float ColorBlendStart166 = _ColorBlendStart;
				float ColorBlendEnd192 = _ColorBlendEnd;
				float4 lerpResult242 = lerp( _FlowerColor1 , _FlowerColor2 , ( saturate( ( ( DistanceToCenter177 - ColorBlendStart166 ) / ColorBlendEnd192 ) ) * step( ColorBlendStart166 , DistanceToCenter177 ) ));
				float4 Color252 = lerpResult242;
				float4 Albedo258 = ( MainTextureColor251 * Color252 );
				
				float Opacity261 = tex2DNode241.a;
				
				
				float3 Albedo = Albedo258.rgb;
				float Alpha = Opacity261;
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
	CustomEditor "StylisedFlowerWithoutStem_MaterialInspector"
	
	
}
/*ASEBEGIN
Version=18600
2352.286;756.1429;1498;761;4619.708;-155.2153;5.009151;True;False
Node;AmplifyShaderEditor.CommentaryNode;137;-2428.854,1283.902;Inherit;False;898.0664;381.0813;;6;176;162;140;139;138;267;Vertex Colors and UVs Baked Data;1,1,1,1;0;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;138;-2371.072,1534.156;Inherit;False;1;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.DynamicAppendNode;139;-2114.743,1541.62;Inherit;False;FLOAT3;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.NegateNode;267;-1943.251,1539.015;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;144;-382.5057,-1541.855;Inherit;False;1913.986;1029.289;;25;261;258;256;255;253;252;251;242;241;237;235;234;227;226;224;221;220;219;207;199;189;177;158;149;147;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;149;-355.4653,-1358.693;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.Vector2Node;147;-337.3897,-1487.105;Inherit;False;Constant;_Vector33;Vector 33;20;0;Create;True;0;0;False;0;False;0.5,0.5;0,0;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RegisterLocalVarNode;140;-1753.743,1535.62;Inherit;False;LocalPivot;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;145;-3066.751,-507.8763;Inherit;False;1401.563;636.4103;;8;187;197;172;161;159;160;143;142;World Space Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;148;-4860.817,-261.0364;Inherit;False;1531.371;1279.004;;37;243;240;238;232;203;201;200;196;195;192;190;186;185;184;182;181;180;179;178;175;174;173;171;170;169;168;166;165;164;163;156;155;154;153;152;151;150;Material Properties;1,1,1,1;0;0
Node;AmplifyShaderEditor.DistanceOpNode;158;-94.28766,-1423.407;Inherit;False;2;0;FLOAT2;0,0;False;1;FLOAT2;0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;142;-2954.363,-465.1791;Inherit;False;140;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;163;-4828.728,-198.8423;Inherit;False;Property;_ColorBlendStart;Color Blend Start;2;0;Create;True;0;0;False;0;False;0.1;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector4Node;161;-2788.459,-167.9963;Inherit;False;Property;_NoiseTextureTilling;Noise Tilling - Static (XY), Animated (ZW);25;0;Create;False;0;0;False;0;False;1,1,1,1;1,1,1,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;156;-4820.564,733.8657;Inherit;False;Property;_MBWindDirBlend;MB Wind Dir Blend;14;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;160;-2712.459,10.00464;Float;False;Property;_NoisePannerSpeed;Noise Panner Speed;26;0;Create;True;0;0;False;0;False;0.05,0.03;0.08,0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.RangedFloatNode;153;-4024.808,23.43872;Float;False;Property;_DBHorizontalPhase;DB Horizontal Phase;19;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.TexturePropertyNode;159;-2734.284,-370.8064;Inherit;True;Property;_NoiseTexture;Noise Texture;24;1;[NoScaleOffset];Create;True;0;0;False;0;False;512fa11ad89d84543ad8d6c8d9cb6743;512fa11ad89d84543ad8d6c8d9cb6743;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RegisterLocalVarNode;177;90.02344,-1429.604;Inherit;False;DistanceToCenter;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.VertexColorNode;162;-2347.06,1337.471;Inherit;False;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;154;-4025.808,118.4387;Float;False;Property;_DBHorizontalMaxRadius;DB Horizontal Max Radius;20;0;Create;True;0;0;False;0;False;0.05;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;151;-4823.564,648.8657;Inherit;False;Global;MBGlobalWindDir;MB Global Wind Dir;28;1;[HideInInspector];Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;155;-4025.808,-173.5613;Float;False;Property;_DBHorizontalAmplitude;DB Horizontal Amplitude;17;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;152;-4025.808,-72.56128;Float;False;Property;_DBHorizontalFrequency;DB Horizontal Frequency;18;0;Create;True;0;0;False;0;False;1.16;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;143;-2743.693,-459.8601;Inherit;False;WorldSpaceUVs - NHP;-1;;109;88a2e8a391a04e241878bdb87d9283a3;0;1;6;FLOAT3;0,0,0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;166;-4297.542,-200.6794;Inherit;False;ColorBlendStart;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;168;-4826.813,-113.7524;Inherit;False;Property;_ColorBlendEnd;Color Blend End;3;0;Create;True;0;0;False;0;False;0.15;1;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;150;-4823.52,566.8867;Float;False;Property;_MBWindDir;MB Wind Dir;12;0;Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;165;-4821.52,469.8867;Float;False;Property;_MBPhase;MB Phase;11;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;176;-1927.444,1428.354;Float;False;DB_PhaseShift;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;181;-4820.865,836.9656;Float;False;Property;_MBWindDirOffset;MB Wind Dir Offset;13;0;Create;True;0;0;False;0;False;20;0;0;180;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;182;-4826.917,183.0166;Float;False;Property;_MBAmplitudeOffset;MB Amplitude Offset;8;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;175;-4825.917,-7.982422;Float;False;Property;_MBDefaultBending;MB Default Bending;6;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;192;-4294.739,-116.0164;Inherit;False;ColorBlendEnd;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;199;59.32458,-1159.576;Inherit;False;166;ColorBlendStart;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;189;45.32458,-1239.576;Inherit;False;177;DistanceToCenter;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;164;-4826.917,279.0166;Float;False;Property;_MBFrequency;MB Frequency;9;0;Create;True;0;0;False;0;False;1.11;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;167;-3070.113,384.2627;Inherit;False;1404.125;639.3857;;10;230;222;216;213;193;191;194;198;202;188;Detail Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;174;-4826.917,87.0166;Float;False;Property;_MBAmplitude;MB Amplitude;7;0;Create;True;0;0;False;0;False;1.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;170;-4819.883,930.2346;Inherit;False;Property;_MBMaxHeight;MB Max Height;15;0;Create;True;0;0;False;0;False;0.5;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;172;-2417.706,-246.3545;Inherit;False;WorldSpaceNoise - NHP;-1;;110;af5fa9ff24e18344ebcc05b64d296c57;0;4;22;FLOAT2;0,0;False;20;SAMPLER2D;;False;24;FLOAT4;1,1,1,1;False;19;FLOAT2;0.1,0.1;False;2;COLOR;0;COLOR;16
Node;AmplifyShaderEditor.LerpOp;180;-4497.564,632.8657;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;169;-3645.808,121.4387;Inherit;False;DB_HorizontalMaxRadius;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;179;-3656.808,-74.56128;Float;False;DB_HorizontalFrequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;171;-3624.808,21.43872;Inherit;False;DB_HorizontalPhase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;178;-3656.808,-170.5613;Float;False;DB_HorizontalAmplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;173;-4824.821,373.1506;Inherit;False;Property;_MBFrequencyOffset;MB Frequency Offset;10;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;197;-1896.027,-293.7273;Inherit;False;StaticNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;203;-4295.674,87.90259;Float;False;MB_Amplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;188;-2992.354,507.9326;Inherit;False;178;DB_HorizontalAmplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;191;-2994.354,585.9326;Inherit;False;179;DB_HorizontalFrequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;207;227.3987,-1078.839;Inherit;False;192;ColorBlendEnd;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;183;-1412.27,-253.3174;Inherit;False;2943.754;1273.023;;33;260;257;266;250;249;247;246;5;2;3;4;0;244;239;236;233;231;229;228;223;225;210;212;204;205;211;206;209;208;217;215;214;218;Main Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;219;296.3246,-1208.576;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;190;-4292.275,470.7737;Inherit;False;MB_Phase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;200;-4329.679,374.1946;Inherit;False;MB_FrequencyOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;202;-2970.812,664.9756;Inherit;False;171;DB_HorizontalPhase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;195;-4327.674,-8.096436;Float;False;MB_DefaultBending;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;194;-2999.639,818.5676;Inherit;False;169;DB_HorizontalMaxRadius;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;184;-4312.52,627.3367;Float;False;MB_WindDirection;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;198;-2939.812,740.9756;Inherit;False;176;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;201;-4295.674,279.9026;Float;False;MB_Frequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;186;-4354.99,837.5486;Inherit;False;MB_WindDirectiionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;187;-1891.718,-178.4744;Inherit;False;AnimatedNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;185;-4327.674,183.9026;Inherit;False;MB_AmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;193;-2925.891,900.0857;Inherit;False;140;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;196;-4295.205,931.1216;Inherit;False;MB_MaxHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;216;-2527.445,481.0916;Float;False;Constant;_Vector2;Vector 2;27;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;224;378.3246,-904.5764;Inherit;False;177;DistanceToCenter;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;209;-1290.015,652.8435;Inherit;False;190;MB_Phase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;213;-2623.724,637.5737;Inherit;False;HorizontalBending - NHP;-1;;111;0b16e2546645f904a949bfd32be36037;0;7;44;FLOAT;1;False;39;FLOAT;1;False;43;FLOAT;1;False;40;FLOAT;0;False;46;FLOAT;2;False;47;FLOAT3;0,0,0;False;45;FLOAT;1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;211;-1374.282,-92.86938;Inherit;False;186;MB_WindDirectiionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;214;-1324.481,745.4646;Inherit;False;176;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;218;-1334.282,-180.8694;Inherit;False;184;MB_WindDirection;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;217;-1347.339,165.8435;Inherit;False;195;MB_DefaultBending;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;205;-1313.282,-3.869385;Inherit;False;187;AnimatedNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;206;-1352.015,360.8435;Inherit;False;185;MB_AmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;208;-1315.015,457.8435;Inherit;False;201;MB_Frequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;220;475.3242,-1143.576;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;221;395.3245,-983.5764;Inherit;False;166;ColorBlendStart;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;215;-1326.943,832.0166;Inherit;False;196;MB_MaxHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;210;-1350.943,551.3125;Inherit;False;200;MB_FrequencyOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;212;-1313.015,261.8435;Inherit;False;203;MB_Amplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;204;-1306.015,919.8435;Inherit;False;197;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;223;-992.7061,465.1077;Inherit;False;RotationAngle - NHP;-1;;114;87b0b7c0fc8f1424db43b84d20c2e79b;0;9;36;FLOAT;0;False;35;FLOAT;0;False;34;FLOAT;1;False;28;FLOAT;1;False;47;FLOAT;0;False;29;FLOAT;1;False;46;FLOAT;0;False;42;FLOAT;0;False;27;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;226;635.3239,-951.5764;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;225;-997.554,-117.3555;Inherit;False;RotationAxis - NHP;-1;;115;b90648f17dcc4bc449d46e8cf04564ff;0;3;20;FLOAT;0;False;19;FLOAT;0;False;18;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.StaticSwitch;222;-2305.056,556.0725;Float;False;Property;_EnableHorizontalBending;Enable Horizontal Bending;16;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SaturateNode;227;619.3239,-1143.576;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;234;797.4258,-1240.324;Inherit;False;Property;_FlowerColor2;Flower Color 2;1;0;Create;True;0;0;False;0;False;0.8980392,0.9529412,1,1;0.009433985,0.4014694,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;228;-548.4219,462.2336;Float;False;MB_RotationAngle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;235;871.3237,-1042.576;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;238;-4023.626,223.4155;Inherit;False;Property;_SlopeCorrectionMagnitude;Slope Correction Magnitude;22;0;Create;False;0;0;False;0;False;1;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;232;-4023.011,317.8516;Inherit;False;Property;_SlopeCorrectionOffset;Slope Correction Offset;23;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;237;795.6246,-1420.541;Inherit;False;Property;_FlowerColor1;Flower Color 1;0;0;Create;True;0;0;False;0;False;0.7843137,0.454902,0.1411765,1;0.5754717,0.3435538,0.1465824,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;229;-554.2828,-121.8694;Inherit;False;MB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;230;-1943.37,555.1646;Float;False;DB_VertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;240;-3668.626,222.4155;Inherit;False;SlopeCorrectionMagnitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;236;-147.4553,210.8677;Inherit;False;140;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;242;1088.149,-1260.854;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;243;-3636.164,317.8596;Inherit;False;SlopeCorrectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;231;-198.8782,13.9375;Inherit;False;229;MB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;233;-202.8782,311.9375;Inherit;False;230;DB_VertexOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;241;-337.3203,-761.2894;Inherit;True;Property;_MainTex;Flower Texture;4;1;[NoScaleOffset];Create;False;0;0;False;1;Header(Textures);False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;239;-209.8782,117.9375;Inherit;False;228;MB_RotationAngle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;247;275.8285,560.6536;Inherit;False;140;LocalPivot;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;244;91.17831,136.5266;Inherit;False;MainBending - NHP;-1;;116;01dba1f3bc33e4b4fa301d2180819576;0;4;55;FLOAT3;0,0,0;False;53;FLOAT;0;False;59;FLOAT3;0,0,0;False;58;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;252;1287.328,-1267.021;Inherit;False;Color;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;249;205.3141,388.6987;Inherit;False;243;SlopeCorrectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;250;184.5338,298.8025;Inherit;False;240;SlopeCorrectionMagnitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;251;30.20184,-762.8074;Inherit;False;MainTextureColor;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;246;265.3141,472.6987;Inherit;False;197;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;253;921.0236,-640.6313;Inherit;False;252;Color;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;255;858.7817,-740.1564;Inherit;False;251;MainTextureColor;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;266;534.2649,341.7107;Inherit;False;SlopeCorrection - NHP;-1;;117;af38de3ca0adf3c4ba9b6a3dd482959e;0;5;87;FLOAT3;0,0,0;False;42;FLOAT;1;False;92;FLOAT;0;False;93;FLOAT4;0,0,0,0;False;41;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;256;1125.402,-698.9653;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.StaticSwitch;257;977.139,131.0557;Float;False;Property;_EnableSlopeCorrection;Enable Slope Correction;21;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;260;1307.049,129.4497;Float;False;LocalVertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;259;1922.719,-767.1108;Inherit;False;759.4954;636.0168;;6;263;265;136;262;1;264;Master Node;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;258;1288.005,-704.8193;Float;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;261;54.17987,-669.7563;Float;False;Opacity;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;265;2082.769,-469.3148;Inherit;False;261;Opacity;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;262;2085.232,-689.147;Inherit;False;258;Albedo;1;0;OBJECT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;263;2032.578,-274.5657;Inherit;False;260;LocalVertexOffset;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;136;2113.558,-579.965;Inherit;False;Constant;_Float2;Float 2;28;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;264;1974.496,-369.6038;Inherit;False;Property;_AlphaCutoff;Alpha Cutoff;5;0;Create;True;0;0;False;0;False;0.5;0.5;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;False;False;False;False;0;False;-1;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;2352.067,-572.4155;Float;False;True;-1;2;StylisedFlowerWithoutStem_MaterialInspector;0;2;Nicrom/NHP/ASE/Stylised Flower Without Stem;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;17;False;False;False;False;False;False;False;False;True;0;False;-1;True;2;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;;0;0;Standard;36;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;0;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;1;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;6,155;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;139;0;138;1
WireConnection;139;2;138;2
WireConnection;267;0;139;0
WireConnection;140;0;267;0
WireConnection;158;0;147;0
WireConnection;158;1;149;0
WireConnection;177;0;158;0
WireConnection;143;6;142;0
WireConnection;166;0;163;0
WireConnection;176;0;162;4
WireConnection;192;0;168;0
WireConnection;172;22;143;0
WireConnection;172;20;159;0
WireConnection;172;24;161;0
WireConnection;172;19;160;0
WireConnection;180;0;150;0
WireConnection;180;1;151;0
WireConnection;180;2;156;0
WireConnection;169;0;154;0
WireConnection;179;0;152;0
WireConnection;171;0;153;0
WireConnection;178;0;155;0
WireConnection;197;0;172;0
WireConnection;203;0;174;0
WireConnection;219;0;189;0
WireConnection;219;1;199;0
WireConnection;190;0;165;0
WireConnection;200;0;173;0
WireConnection;195;0;175;0
WireConnection;184;0;180;0
WireConnection;201;0;164;0
WireConnection;186;0;181;0
WireConnection;187;0;172;16
WireConnection;185;0;182;0
WireConnection;196;0;170;0
WireConnection;213;44;188;0
WireConnection;213;39;191;0
WireConnection;213;43;202;0
WireConnection;213;40;198;0
WireConnection;213;46;194;0
WireConnection;213;47;193;0
WireConnection;220;0;219;0
WireConnection;220;1;207;0
WireConnection;223;36;217;0
WireConnection;223;35;212;0
WireConnection;223;34;206;0
WireConnection;223;28;208;0
WireConnection;223;47;210;0
WireConnection;223;29;209;0
WireConnection;223;46;214;0
WireConnection;223;42;215;0
WireConnection;223;27;204;0
WireConnection;226;0;221;0
WireConnection;226;1;224;0
WireConnection;225;20;218;0
WireConnection;225;19;211;0
WireConnection;225;18;205;0
WireConnection;222;1;216;0
WireConnection;222;0;213;0
WireConnection;227;0;220;0
WireConnection;228;0;223;0
WireConnection;235;0;227;0
WireConnection;235;1;226;0
WireConnection;229;0;225;0
WireConnection;230;0;222;0
WireConnection;240;0;238;0
WireConnection;242;0;237;0
WireConnection;242;1;234;0
WireConnection;242;2;235;0
WireConnection;243;0;232;0
WireConnection;244;55;231;0
WireConnection;244;53;239;0
WireConnection;244;59;236;0
WireConnection;244;58;233;0
WireConnection;252;0;242;0
WireConnection;251;0;241;0
WireConnection;266;87;244;0
WireConnection;266;42;250;0
WireConnection;266;92;249;0
WireConnection;266;93;246;0
WireConnection;266;41;247;0
WireConnection;256;0;255;0
WireConnection;256;1;253;0
WireConnection;257;1;244;0
WireConnection;257;0;266;0
WireConnection;260;0;257;0
WireConnection;258;0;256;0
WireConnection;261;0;241;4
WireConnection;1;0;262;0
WireConnection;1;3;136;0
WireConnection;1;4;136;0
WireConnection;1;6;265;0
WireConnection;1;7;264;0
WireConnection;1;8;263;0
ASEEND*/
//CHKSM=01EAE0172049B97FB04A3C747C306674C8F7D102