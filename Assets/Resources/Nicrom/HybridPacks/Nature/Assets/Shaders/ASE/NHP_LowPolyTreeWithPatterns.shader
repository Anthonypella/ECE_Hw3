// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Nicrom/NHP/ASE/Low Poly Tree With Patterns"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin]_Metallic("Metallic", Range( 0 , 1)) = 0
		_Smoothness("Smoothness", Range( 0 , 1)) = 0
		_PatternColor1("Pattern Color 1", Color) = (1,1,1,1)
		_PatternColor2("Pattern Color 2", Color) = (0,0,0,1)
		[NoScaleOffset]_MainTex("Main Texture", 2D) = "white" {}
		[NoScaleOffset]_PatternTex("Pattern Texture", 2D) = "white" {}
		[NoScaleOffset]_NoiseTexture("Noise Texture", 2D) = "white" {}
		_NoiseTextureTilling("Noise Tilling - Static (XY), Animated (ZW)", Vector) = (1,1,1,1)
		_NoisePannerSpeed("Noise Panner Speed", Vector) = (0.1,0.1,0,0)
		_MBDefaultBending("MB Default Bending", Float) = 0
		_MBAmplitude("MB Amplitude", Float) = 1.5
		_MBAmplitudeOffset("MB Amplitude Offset", Float) = 2
		_MBFrequency("MB Frequency", Float) = 1.11
		_MBFrequencyOffset("MB Frequency Offset", Float) = 0
		_MBPhase("MB Phase", Float) = 1
		_MBWindDirBlend("MB Wind Dir Blend", Range( 0 , 1)) = 0
		_MBWindDir("MB Wind Dir", Range( 0 , 360)) = 0
		_MBWindDirOffset("MB Wind Dir Offset", Range( 0 , 180)) = 20
		_MBMaxHeight("MB Max Height", Float) = 10
		[Toggle(_ENABLEVERTICALBENDING_ON)] _EnableVerticalBending("Enable Vertical Bending", Float) = 1
		_DBVerticalAmplitude("DB Vertical Amplitude", Float) = 1
		_DBVerticalAmplitudeOffset("DB Vertical Amplitude Offset", Float) = 1.2
		_DBVerticalFrequency("DB Vertical Frequency", Float) = 1.15
		_DBVerticalPhase("DB Vertical Phase", Float) = 1
		_DBVerticalMaxLength("DB Vertical Max Length", Float) = 2
		[Toggle(_ENABLEHORIZONTALBENDING_ON)] _EnableHorizontalBending("Enable Horizontal Bending", Float) = 1
		_DBHorizontalAmplitude("DB Horizontal Amplitude", Float) = 2
		_DBHorizontalFrequency("DB Horizontal Frequency", Float) = 1.16
		_DBHorizontalPhase("DB Horizontal Phase", Float) = 1
		_DBHorizontalMaxRadius("DB Horizontal Max Radius", Float) = 2
		[ASEEnd]_UnitScale("Unit Scale", Float) = 20

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
			#pragma shader_feature_local _ENABLEVERTICALBENDING_ON
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
			float4 _PatternTex_ST;
			float4 _PatternColor1;
			float4 _NoiseTextureTilling;
			float4 _PatternColor2;
			float4 _MainTex_ST;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalAmplitudeOffset;
			float _Metallic;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBVerticalFrequency;
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
			sampler2D _PatternTex;


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

				float lerpResult273 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection209 = lerpResult273;
				float MB_WindDirectionOffset215 = _MBWindDirOffset;
				float4 transform181 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult192 = (float2(transform181.x , transform181.z));
				float2 UVs27_g96 = appendResult192;
				float4 temp_output_24_0_g96 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g96 = (temp_output_24_0_g96).zw;
				float2 panner7_g96 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g96 * AnimatedNoiseTilling29_g96 ) + panner7_g96 ), 0, 0.0) );
				float temp_output_11_0_g99 = radians( ( ( MB_WindDirection209 + ( MB_WindDirectionOffset215 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g99 = (float3(cos( temp_output_11_0_g99 ) , 0.0 , sin( temp_output_11_0_g99 )));
				float4 transform15_g99 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g99 , 0.0 ));
				float3 normalizeResult34_g99 = normalize( (transform15_g99).xyz );
				float3 MB_RotationAxis244 = normalizeResult34_g99;
				float3 RotationAxis56_g100 = MB_RotationAxis244;
				float MB_Amplitude208 = _MBAmplitude;
				float MB_AmplitudeOffset214 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g96 = (temp_output_24_0_g96).xy;
				float4 StaticNoise206 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g96 * StaticNoileTilling28_g96 ), 0, 0.0) );
				float4 StaticWorldNoise31_g98 = StaticNoise206;
				float4 transform8_g98 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency219 = _MBFrequency;
				float MB_FrequencyOffset213 = _MBFrequencyOffset;
				float MB_Phase220 = _MBPhase;
				float MB_DefaultBending217 = _MBDefaultBending;
				float MB_MaxHeight221 = _MBMaxHeight;
				float MB_RotationAngle247 = radians( ( ( ( ( MB_Amplitude208 + ( MB_AmplitudeOffset214 * (StaticWorldNoise31_g98).x ) ) * sin( ( ( ( transform8_g98.x + transform8_g98.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency219 + ( MB_FrequencyOffset213 * (StaticWorldNoise31_g98).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase220 ) ) ) + MB_DefaultBending217 ) * ( v.vertex.xyz.y / MB_MaxHeight221 ) ) );
				float RotationAngle54_g100 = MB_RotationAngle247;
				float3 PivotPoint60_g100 = float3( 0,0,0 );
				float3 break62_g100 = PivotPoint60_g100;
				float3 appendResult45_g100 = (float3(break62_g100.x , v.vertex.xyz.y , break62_g100.z));
				float3 rotatedValue30_g100 = RotateAroundAxis( appendResult45_g100, v.vertex.xyz, RotationAxis56_g100, RotationAngle54_g100 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis170 = appendResult10_g90;
				float DB_VerticalAmplitude160 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset168 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift171 = v.ase_color.a;
				float PhaseShift48_g95 = DB_PhaseShift171;
				float DB_VerticalFrequency166 = _DBVerticalFrequency;
				float Fequency45_g95 = DB_VerticalFrequency166;
				float4 transform2_g95 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase159 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g95 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength167 = _DBVerticalMaxLength;
				float3 rotatedValue29_g95 = RotateAroundAxis( PivotPosOnYAxis56_g95, v.vertex.xyz, DB_RotationAxis170, radians( ( ( ( DB_VerticalAmplitude160 + ( DB_VerticalAmplitudeOffset168 * ( 1.0 - PhaseShift48_g95 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g95 ) - ( ( 2.0 * PI ) * PhaseShift48_g95 ) ) + ( ( ( transform2_g95.x + transform2_g95.z ) + ( ( _TimeParameters.x ) * Fequency45_g95 ) ) * DB_VerticalPhase159 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g95 ) / DB_VerticalMaxLength167 ) ) ) );
				float DetailBendingMask165 = step( 1.5 , v.texcoord.x );
				float3 DB_VerticalBending201 = ( ( rotatedValue29_g95 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch225 = DB_VerticalBending201;
				#else
				float3 staticSwitch225 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude161 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency162 = _DBHorizontalFrequency;
				float Frequency41_g94 = DB_HorizontalFrequency162;
				float4 transform5_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase172 = _DBHorizontalPhase;
				float3 PivotPoint49_g94 = float3( 0,0,0 );
				float3 break52_g94 = PivotPoint49_g94;
				float3 appendResult20_g94 = (float3(break52_g94.x , v.vertex.xyz.y , break52_g94.z));
				float DB_HorizontalMaxRadius158 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g94 = RotateAroundAxis( PivotPoint49_g94, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude161 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g94 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift171 ) ) ) + ( ( ( transform5_g94.x + transform5_g94.z ) + ( ( _TimeParameters.x ) * Frequency41_g94 ) ) * DB_HorizontalPhase172 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g94 ) / DB_HorizontalMaxRadius158 ) ) ) );
				float3 DB_HorizontalBending202 = ( ( rotatedValue33_g94 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch226 = DB_HorizontalBending202;
				#else
				float3 staticSwitch226 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset243 = ( staticSwitch225 + staticSwitch226 );
				float3 rotatedValue34_g100 = RotateAroundAxis( PivotPoint60_g100, ( rotatedValue30_g100 + DB_VertexOffset243 ), RotationAxis56_g100, RotationAngle54_g100 );
				float3 LocalVertexOffset263 = ( ( rotatedValue34_g100 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset263;
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
				float2 uv_PatternTex = IN.ase_texcoord7.xy * _PatternTex_ST.xy + _PatternTex_ST.zw;
				float4 lerpResult257 = lerp( _PatternColor2 , _PatternColor1 , tex2D( _PatternTex, uv_PatternTex ));
				float TextureMask253 = step( IN.ase_texcoord7.xy.y , -1.0 );
				float4 lerpResult262 = lerp( tex2D( _MainTex, uv_MainTex ) , lerpResult257 , TextureMask253);
				float4 Albedo264 = lerpResult262;
				
				float3 Albedo = Albedo264.rgb;
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
			#pragma shader_feature_local _ENABLEVERTICALBENDING_ON
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
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
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _PatternTex_ST;
			float4 _PatternColor1;
			float4 _NoiseTextureTilling;
			float4 _PatternColor2;
			float4 _MainTex_ST;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalAmplitudeOffset;
			float _Metallic;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBVerticalFrequency;
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

				float lerpResult273 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection209 = lerpResult273;
				float MB_WindDirectionOffset215 = _MBWindDirOffset;
				float4 transform181 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult192 = (float2(transform181.x , transform181.z));
				float2 UVs27_g96 = appendResult192;
				float4 temp_output_24_0_g96 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g96 = (temp_output_24_0_g96).zw;
				float2 panner7_g96 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g96 * AnimatedNoiseTilling29_g96 ) + panner7_g96 ), 0, 0.0) );
				float temp_output_11_0_g99 = radians( ( ( MB_WindDirection209 + ( MB_WindDirectionOffset215 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g99 = (float3(cos( temp_output_11_0_g99 ) , 0.0 , sin( temp_output_11_0_g99 )));
				float4 transform15_g99 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g99 , 0.0 ));
				float3 normalizeResult34_g99 = normalize( (transform15_g99).xyz );
				float3 MB_RotationAxis244 = normalizeResult34_g99;
				float3 RotationAxis56_g100 = MB_RotationAxis244;
				float MB_Amplitude208 = _MBAmplitude;
				float MB_AmplitudeOffset214 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g96 = (temp_output_24_0_g96).xy;
				float4 StaticNoise206 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g96 * StaticNoileTilling28_g96 ), 0, 0.0) );
				float4 StaticWorldNoise31_g98 = StaticNoise206;
				float4 transform8_g98 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency219 = _MBFrequency;
				float MB_FrequencyOffset213 = _MBFrequencyOffset;
				float MB_Phase220 = _MBPhase;
				float MB_DefaultBending217 = _MBDefaultBending;
				float MB_MaxHeight221 = _MBMaxHeight;
				float MB_RotationAngle247 = radians( ( ( ( ( MB_Amplitude208 + ( MB_AmplitudeOffset214 * (StaticWorldNoise31_g98).x ) ) * sin( ( ( ( transform8_g98.x + transform8_g98.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency219 + ( MB_FrequencyOffset213 * (StaticWorldNoise31_g98).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase220 ) ) ) + MB_DefaultBending217 ) * ( v.vertex.xyz.y / MB_MaxHeight221 ) ) );
				float RotationAngle54_g100 = MB_RotationAngle247;
				float3 PivotPoint60_g100 = float3( 0,0,0 );
				float3 break62_g100 = PivotPoint60_g100;
				float3 appendResult45_g100 = (float3(break62_g100.x , v.vertex.xyz.y , break62_g100.z));
				float3 rotatedValue30_g100 = RotateAroundAxis( appendResult45_g100, v.vertex.xyz, RotationAxis56_g100, RotationAngle54_g100 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis170 = appendResult10_g90;
				float DB_VerticalAmplitude160 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset168 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift171 = v.ase_color.a;
				float PhaseShift48_g95 = DB_PhaseShift171;
				float DB_VerticalFrequency166 = _DBVerticalFrequency;
				float Fequency45_g95 = DB_VerticalFrequency166;
				float4 transform2_g95 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase159 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g95 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength167 = _DBVerticalMaxLength;
				float3 rotatedValue29_g95 = RotateAroundAxis( PivotPosOnYAxis56_g95, v.vertex.xyz, DB_RotationAxis170, radians( ( ( ( DB_VerticalAmplitude160 + ( DB_VerticalAmplitudeOffset168 * ( 1.0 - PhaseShift48_g95 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g95 ) - ( ( 2.0 * PI ) * PhaseShift48_g95 ) ) + ( ( ( transform2_g95.x + transform2_g95.z ) + ( ( _TimeParameters.x ) * Fequency45_g95 ) ) * DB_VerticalPhase159 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g95 ) / DB_VerticalMaxLength167 ) ) ) );
				float DetailBendingMask165 = step( 1.5 , v.ase_texcoord.x );
				float3 DB_VerticalBending201 = ( ( rotatedValue29_g95 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch225 = DB_VerticalBending201;
				#else
				float3 staticSwitch225 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude161 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency162 = _DBHorizontalFrequency;
				float Frequency41_g94 = DB_HorizontalFrequency162;
				float4 transform5_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase172 = _DBHorizontalPhase;
				float3 PivotPoint49_g94 = float3( 0,0,0 );
				float3 break52_g94 = PivotPoint49_g94;
				float3 appendResult20_g94 = (float3(break52_g94.x , v.vertex.xyz.y , break52_g94.z));
				float DB_HorizontalMaxRadius158 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g94 = RotateAroundAxis( PivotPoint49_g94, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude161 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g94 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift171 ) ) ) + ( ( ( transform5_g94.x + transform5_g94.z ) + ( ( _TimeParameters.x ) * Frequency41_g94 ) ) * DB_HorizontalPhase172 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g94 ) / DB_HorizontalMaxRadius158 ) ) ) );
				float3 DB_HorizontalBending202 = ( ( rotatedValue33_g94 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch226 = DB_HorizontalBending202;
				#else
				float3 staticSwitch226 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset243 = ( staticSwitch225 + staticSwitch226 );
				float3 rotatedValue34_g100 = RotateAroundAxis( PivotPoint60_g100, ( rotatedValue30_g100 + DB_VertexOffset243 ), RotationAxis56_g100, RotationAngle54_g100 );
				float3 LocalVertexOffset263 = ( ( rotatedValue34_g100 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset263;
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
			#pragma shader_feature_local _ENABLEVERTICALBENDING_ON
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
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
				
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			CBUFFER_START(UnityPerMaterial)
			float4 _PatternTex_ST;
			float4 _PatternColor1;
			float4 _NoiseTextureTilling;
			float4 _PatternColor2;
			float4 _MainTex_ST;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalAmplitudeOffset;
			float _Metallic;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBVerticalFrequency;
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

				float lerpResult273 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection209 = lerpResult273;
				float MB_WindDirectionOffset215 = _MBWindDirOffset;
				float4 transform181 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult192 = (float2(transform181.x , transform181.z));
				float2 UVs27_g96 = appendResult192;
				float4 temp_output_24_0_g96 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g96 = (temp_output_24_0_g96).zw;
				float2 panner7_g96 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g96 * AnimatedNoiseTilling29_g96 ) + panner7_g96 ), 0, 0.0) );
				float temp_output_11_0_g99 = radians( ( ( MB_WindDirection209 + ( MB_WindDirectionOffset215 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g99 = (float3(cos( temp_output_11_0_g99 ) , 0.0 , sin( temp_output_11_0_g99 )));
				float4 transform15_g99 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g99 , 0.0 ));
				float3 normalizeResult34_g99 = normalize( (transform15_g99).xyz );
				float3 MB_RotationAxis244 = normalizeResult34_g99;
				float3 RotationAxis56_g100 = MB_RotationAxis244;
				float MB_Amplitude208 = _MBAmplitude;
				float MB_AmplitudeOffset214 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g96 = (temp_output_24_0_g96).xy;
				float4 StaticNoise206 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g96 * StaticNoileTilling28_g96 ), 0, 0.0) );
				float4 StaticWorldNoise31_g98 = StaticNoise206;
				float4 transform8_g98 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency219 = _MBFrequency;
				float MB_FrequencyOffset213 = _MBFrequencyOffset;
				float MB_Phase220 = _MBPhase;
				float MB_DefaultBending217 = _MBDefaultBending;
				float MB_MaxHeight221 = _MBMaxHeight;
				float MB_RotationAngle247 = radians( ( ( ( ( MB_Amplitude208 + ( MB_AmplitudeOffset214 * (StaticWorldNoise31_g98).x ) ) * sin( ( ( ( transform8_g98.x + transform8_g98.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency219 + ( MB_FrequencyOffset213 * (StaticWorldNoise31_g98).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase220 ) ) ) + MB_DefaultBending217 ) * ( v.vertex.xyz.y / MB_MaxHeight221 ) ) );
				float RotationAngle54_g100 = MB_RotationAngle247;
				float3 PivotPoint60_g100 = float3( 0,0,0 );
				float3 break62_g100 = PivotPoint60_g100;
				float3 appendResult45_g100 = (float3(break62_g100.x , v.vertex.xyz.y , break62_g100.z));
				float3 rotatedValue30_g100 = RotateAroundAxis( appendResult45_g100, v.vertex.xyz, RotationAxis56_g100, RotationAngle54_g100 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis170 = appendResult10_g90;
				float DB_VerticalAmplitude160 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset168 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift171 = v.ase_color.a;
				float PhaseShift48_g95 = DB_PhaseShift171;
				float DB_VerticalFrequency166 = _DBVerticalFrequency;
				float Fequency45_g95 = DB_VerticalFrequency166;
				float4 transform2_g95 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase159 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g95 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength167 = _DBVerticalMaxLength;
				float3 rotatedValue29_g95 = RotateAroundAxis( PivotPosOnYAxis56_g95, v.vertex.xyz, DB_RotationAxis170, radians( ( ( ( DB_VerticalAmplitude160 + ( DB_VerticalAmplitudeOffset168 * ( 1.0 - PhaseShift48_g95 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g95 ) - ( ( 2.0 * PI ) * PhaseShift48_g95 ) ) + ( ( ( transform2_g95.x + transform2_g95.z ) + ( ( _TimeParameters.x ) * Fequency45_g95 ) ) * DB_VerticalPhase159 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g95 ) / DB_VerticalMaxLength167 ) ) ) );
				float DetailBendingMask165 = step( 1.5 , v.ase_texcoord.x );
				float3 DB_VerticalBending201 = ( ( rotatedValue29_g95 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch225 = DB_VerticalBending201;
				#else
				float3 staticSwitch225 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude161 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency162 = _DBHorizontalFrequency;
				float Frequency41_g94 = DB_HorizontalFrequency162;
				float4 transform5_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase172 = _DBHorizontalPhase;
				float3 PivotPoint49_g94 = float3( 0,0,0 );
				float3 break52_g94 = PivotPoint49_g94;
				float3 appendResult20_g94 = (float3(break52_g94.x , v.vertex.xyz.y , break52_g94.z));
				float DB_HorizontalMaxRadius158 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g94 = RotateAroundAxis( PivotPoint49_g94, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude161 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g94 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift171 ) ) ) + ( ( ( transform5_g94.x + transform5_g94.z ) + ( ( _TimeParameters.x ) * Frequency41_g94 ) ) * DB_HorizontalPhase172 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g94 ) / DB_HorizontalMaxRadius158 ) ) ) );
				float3 DB_HorizontalBending202 = ( ( rotatedValue33_g94 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch226 = DB_HorizontalBending202;
				#else
				float3 staticSwitch226 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset243 = ( staticSwitch225 + staticSwitch226 );
				float3 rotatedValue34_g100 = RotateAroundAxis( PivotPoint60_g100, ( rotatedValue30_g100 + DB_VertexOffset243 ), RotationAxis56_g100, RotationAngle54_g100 );
				float3 LocalVertexOffset263 = ( ( rotatedValue34_g100 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset263;
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
			#pragma shader_feature_local _ENABLEVERTICALBENDING_ON
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
			float4 _PatternTex_ST;
			float4 _PatternColor1;
			float4 _NoiseTextureTilling;
			float4 _PatternColor2;
			float4 _MainTex_ST;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalAmplitudeOffset;
			float _Metallic;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBVerticalFrequency;
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
			sampler2D _PatternTex;


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

				float lerpResult273 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection209 = lerpResult273;
				float MB_WindDirectionOffset215 = _MBWindDirOffset;
				float4 transform181 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult192 = (float2(transform181.x , transform181.z));
				float2 UVs27_g96 = appendResult192;
				float4 temp_output_24_0_g96 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g96 = (temp_output_24_0_g96).zw;
				float2 panner7_g96 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g96 * AnimatedNoiseTilling29_g96 ) + panner7_g96 ), 0, 0.0) );
				float temp_output_11_0_g99 = radians( ( ( MB_WindDirection209 + ( MB_WindDirectionOffset215 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g99 = (float3(cos( temp_output_11_0_g99 ) , 0.0 , sin( temp_output_11_0_g99 )));
				float4 transform15_g99 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g99 , 0.0 ));
				float3 normalizeResult34_g99 = normalize( (transform15_g99).xyz );
				float3 MB_RotationAxis244 = normalizeResult34_g99;
				float3 RotationAxis56_g100 = MB_RotationAxis244;
				float MB_Amplitude208 = _MBAmplitude;
				float MB_AmplitudeOffset214 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g96 = (temp_output_24_0_g96).xy;
				float4 StaticNoise206 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g96 * StaticNoileTilling28_g96 ), 0, 0.0) );
				float4 StaticWorldNoise31_g98 = StaticNoise206;
				float4 transform8_g98 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency219 = _MBFrequency;
				float MB_FrequencyOffset213 = _MBFrequencyOffset;
				float MB_Phase220 = _MBPhase;
				float MB_DefaultBending217 = _MBDefaultBending;
				float MB_MaxHeight221 = _MBMaxHeight;
				float MB_RotationAngle247 = radians( ( ( ( ( MB_Amplitude208 + ( MB_AmplitudeOffset214 * (StaticWorldNoise31_g98).x ) ) * sin( ( ( ( transform8_g98.x + transform8_g98.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency219 + ( MB_FrequencyOffset213 * (StaticWorldNoise31_g98).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase220 ) ) ) + MB_DefaultBending217 ) * ( v.vertex.xyz.y / MB_MaxHeight221 ) ) );
				float RotationAngle54_g100 = MB_RotationAngle247;
				float3 PivotPoint60_g100 = float3( 0,0,0 );
				float3 break62_g100 = PivotPoint60_g100;
				float3 appendResult45_g100 = (float3(break62_g100.x , v.vertex.xyz.y , break62_g100.z));
				float3 rotatedValue30_g100 = RotateAroundAxis( appendResult45_g100, v.vertex.xyz, RotationAxis56_g100, RotationAngle54_g100 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis170 = appendResult10_g90;
				float DB_VerticalAmplitude160 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset168 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift171 = v.ase_color.a;
				float PhaseShift48_g95 = DB_PhaseShift171;
				float DB_VerticalFrequency166 = _DBVerticalFrequency;
				float Fequency45_g95 = DB_VerticalFrequency166;
				float4 transform2_g95 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase159 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g95 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength167 = _DBVerticalMaxLength;
				float3 rotatedValue29_g95 = RotateAroundAxis( PivotPosOnYAxis56_g95, v.vertex.xyz, DB_RotationAxis170, radians( ( ( ( DB_VerticalAmplitude160 + ( DB_VerticalAmplitudeOffset168 * ( 1.0 - PhaseShift48_g95 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g95 ) - ( ( 2.0 * PI ) * PhaseShift48_g95 ) ) + ( ( ( transform2_g95.x + transform2_g95.z ) + ( ( _TimeParameters.x ) * Fequency45_g95 ) ) * DB_VerticalPhase159 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g95 ) / DB_VerticalMaxLength167 ) ) ) );
				float DetailBendingMask165 = step( 1.5 , v.ase_texcoord.x );
				float3 DB_VerticalBending201 = ( ( rotatedValue29_g95 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch225 = DB_VerticalBending201;
				#else
				float3 staticSwitch225 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude161 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency162 = _DBHorizontalFrequency;
				float Frequency41_g94 = DB_HorizontalFrequency162;
				float4 transform5_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase172 = _DBHorizontalPhase;
				float3 PivotPoint49_g94 = float3( 0,0,0 );
				float3 break52_g94 = PivotPoint49_g94;
				float3 appendResult20_g94 = (float3(break52_g94.x , v.vertex.xyz.y , break52_g94.z));
				float DB_HorizontalMaxRadius158 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g94 = RotateAroundAxis( PivotPoint49_g94, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude161 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g94 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift171 ) ) ) + ( ( ( transform5_g94.x + transform5_g94.z ) + ( ( _TimeParameters.x ) * Frequency41_g94 ) ) * DB_HorizontalPhase172 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g94 ) / DB_HorizontalMaxRadius158 ) ) ) );
				float3 DB_HorizontalBending202 = ( ( rotatedValue33_g94 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch226 = DB_HorizontalBending202;
				#else
				float3 staticSwitch226 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset243 = ( staticSwitch225 + staticSwitch226 );
				float3 rotatedValue34_g100 = RotateAroundAxis( PivotPoint60_g100, ( rotatedValue30_g100 + DB_VertexOffset243 ), RotationAxis56_g100, RotationAngle54_g100 );
				float3 LocalVertexOffset263 = ( ( rotatedValue34_g100 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset263;
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
				float2 uv_PatternTex = IN.ase_texcoord2.xy * _PatternTex_ST.xy + _PatternTex_ST.zw;
				float4 lerpResult257 = lerp( _PatternColor2 , _PatternColor1 , tex2D( _PatternTex, uv_PatternTex ));
				float TextureMask253 = step( IN.ase_texcoord2.xy.y , -1.0 );
				float4 lerpResult262 = lerp( tex2D( _MainTex, uv_MainTex ) , lerpResult257 , TextureMask253);
				float4 Albedo264 = lerpResult262;
				
				
				float3 Albedo = Albedo264.rgb;
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
			#pragma shader_feature_local _ENABLEVERTICALBENDING_ON
			#pragma shader_feature_local _ENABLEHORIZONTALBENDING_ON


			#pragma shader_feature _ _SMOOTHNESS_TEXTURE_ALBEDO_CHANNEL_A

			struct VertexInput
			{
				float4 vertex : POSITION;
				float3 ase_normal : NORMAL;
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
			float4 _PatternTex_ST;
			float4 _PatternColor1;
			float4 _NoiseTextureTilling;
			float4 _PatternColor2;
			float4 _MainTex_ST;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalAmplitudeOffset;
			float _Metallic;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindDirBlend;
			float _DBVerticalFrequency;
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
			sampler2D _PatternTex;


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

				float lerpResult273 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindDirBlend);
				float MB_WindDirection209 = lerpResult273;
				float MB_WindDirectionOffset215 = _MBWindDirOffset;
				float4 transform181 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult192 = (float2(transform181.x , transform181.z));
				float2 UVs27_g96 = appendResult192;
				float4 temp_output_24_0_g96 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g96 = (temp_output_24_0_g96).zw;
				float2 panner7_g96 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedNoise216 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g96 * AnimatedNoiseTilling29_g96 ) + panner7_g96 ), 0, 0.0) );
				float temp_output_11_0_g99 = radians( ( ( MB_WindDirection209 + ( MB_WindDirectionOffset215 * (-1.0 + ((AnimatedNoise216).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g99 = (float3(cos( temp_output_11_0_g99 ) , 0.0 , sin( temp_output_11_0_g99 )));
				float4 transform15_g99 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g99 , 0.0 ));
				float3 normalizeResult34_g99 = normalize( (transform15_g99).xyz );
				float3 MB_RotationAxis244 = normalizeResult34_g99;
				float3 RotationAxis56_g100 = MB_RotationAxis244;
				float MB_Amplitude208 = _MBAmplitude;
				float MB_AmplitudeOffset214 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g96 = (temp_output_24_0_g96).xy;
				float4 StaticNoise206 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g96 * StaticNoileTilling28_g96 ), 0, 0.0) );
				float4 StaticWorldNoise31_g98 = StaticNoise206;
				float4 transform8_g98 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency219 = _MBFrequency;
				float MB_FrequencyOffset213 = _MBFrequencyOffset;
				float MB_Phase220 = _MBPhase;
				float MB_DefaultBending217 = _MBDefaultBending;
				float MB_MaxHeight221 = _MBMaxHeight;
				float MB_RotationAngle247 = radians( ( ( ( ( MB_Amplitude208 + ( MB_AmplitudeOffset214 * (StaticWorldNoise31_g98).x ) ) * sin( ( ( ( transform8_g98.x + transform8_g98.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency219 + ( MB_FrequencyOffset213 * (StaticWorldNoise31_g98).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase220 ) ) ) + MB_DefaultBending217 ) * ( v.vertex.xyz.y / MB_MaxHeight221 ) ) );
				float RotationAngle54_g100 = MB_RotationAngle247;
				float3 PivotPoint60_g100 = float3( 0,0,0 );
				float3 break62_g100 = PivotPoint60_g100;
				float3 appendResult45_g100 = (float3(break62_g100.x , v.vertex.xyz.y , break62_g100.z));
				float3 rotatedValue30_g100 = RotateAroundAxis( appendResult45_g100, v.vertex.xyz, RotationAxis56_g100, RotationAngle54_g100 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis170 = appendResult10_g90;
				float DB_VerticalAmplitude160 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset168 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift171 = v.ase_color.a;
				float PhaseShift48_g95 = DB_PhaseShift171;
				float DB_VerticalFrequency166 = _DBVerticalFrequency;
				float Fequency45_g95 = DB_VerticalFrequency166;
				float4 transform2_g95 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase159 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g95 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength167 = _DBVerticalMaxLength;
				float3 rotatedValue29_g95 = RotateAroundAxis( PivotPosOnYAxis56_g95, v.vertex.xyz, DB_RotationAxis170, radians( ( ( ( DB_VerticalAmplitude160 + ( DB_VerticalAmplitudeOffset168 * ( 1.0 - PhaseShift48_g95 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g95 ) - ( ( 2.0 * PI ) * PhaseShift48_g95 ) ) + ( ( ( transform2_g95.x + transform2_g95.z ) + ( ( _TimeParameters.x ) * Fequency45_g95 ) ) * DB_VerticalPhase159 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g95 ) / DB_VerticalMaxLength167 ) ) ) );
				float DetailBendingMask165 = step( 1.5 , v.ase_texcoord.x );
				float3 DB_VerticalBending201 = ( ( rotatedValue29_g95 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch225 = DB_VerticalBending201;
				#else
				float3 staticSwitch225 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude161 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency162 = _DBHorizontalFrequency;
				float Frequency41_g94 = DB_HorizontalFrequency162;
				float4 transform5_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase172 = _DBHorizontalPhase;
				float3 PivotPoint49_g94 = float3( 0,0,0 );
				float3 break52_g94 = PivotPoint49_g94;
				float3 appendResult20_g94 = (float3(break52_g94.x , v.vertex.xyz.y , break52_g94.z));
				float DB_HorizontalMaxRadius158 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g94 = RotateAroundAxis( PivotPoint49_g94, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude161 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g94 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift171 ) ) ) + ( ( ( transform5_g94.x + transform5_g94.z ) + ( ( _TimeParameters.x ) * Frequency41_g94 ) ) * DB_HorizontalPhase172 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g94 ) / DB_HorizontalMaxRadius158 ) ) ) );
				float3 DB_HorizontalBending202 = ( ( rotatedValue33_g94 - v.vertex.xyz ) * DetailBendingMask165 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch226 = DB_HorizontalBending202;
				#else
				float3 staticSwitch226 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset243 = ( staticSwitch225 + staticSwitch226 );
				float3 rotatedValue34_g100 = RotateAroundAxis( PivotPoint60_g100, ( rotatedValue30_g100 + DB_VertexOffset243 ), RotationAxis56_g100, RotationAngle54_g100 );
				float3 LocalVertexOffset263 = ( ( rotatedValue34_g100 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset263;
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
				float2 uv_PatternTex = IN.ase_texcoord2.xy * _PatternTex_ST.xy + _PatternTex_ST.zw;
				float4 lerpResult257 = lerp( _PatternColor2 , _PatternColor1 , tex2D( _PatternTex, uv_PatternTex ));
				float TextureMask253 = step( IN.ase_texcoord2.xy.y , -1.0 );
				float4 lerpResult262 = lerp( tex2D( _MainTex, uv_MainTex ) , lerpResult257 , TextureMask253);
				float4 Albedo264 = lerpResult262;
				
				
				float3 Albedo = Albedo264.rgb;
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
	CustomEditor "LowPolyTreeWithPatterns_MaterialInspector"
	
	
}
/*ASEBEGIN
Version=18600
2582.286;1000.143;1083;565;3158.123;527.6168;5.268732;True;False
Node;AmplifyShaderEditor.CommentaryNode;142;-2172.666,2433.24;Inherit;False;888.8715;764.828;;13;253;245;241;238;171;170;165;163;155;153;146;144;143;Vertex Colors and UVs Baked Data;1,1,1,1;0;0
Node;AmplifyShaderEditor.RangedFloatNode;144;-2150.419,2535.304;Float;False;Property;_UnitScale;Unit Scale;30;0;Create;True;0;0;False;0;False;20;20;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;143;-2080.9,2763.35;Inherit;False;Constant;_Float1;Float 1;26;0;Create;True;0;0;False;0;False;1.5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;145;-2811.844,1025.644;Inherit;False;1524.165;1150.683;;39;273;272;271;270;221;220;219;217;215;214;213;209;208;204;203;200;199;198;197;196;195;172;168;167;166;162;161;160;159;158;157;156;154;152;151;150;149;148;147;Material Properties;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;146;-2127.046,2846.31;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;149;-1928.477,1607.426;Float;False;Property;_DBHorizontalAmplitude;DB Horizontal Amplitude;26;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;156;-1928.477,1480.426;Float;False;Property;_DBVerticalMaxLength;DB Vertical Max Length;24;0;Create;True;0;0;False;0;False;2;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;157;-1927.477,1383.426;Float;False;Property;_DBVerticalPhase;DB Vertical Phase;23;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;155;-1965.043,2539.621;Inherit;False;VertexColorData - NHP;-1;;90;0242ce46c610b224e91bc03a7bf52b77;0;1;17;FLOAT;0;False;3;FLOAT3;19;FLOAT3;0;FLOAT;18
Node;AmplifyShaderEditor.RangedFloatNode;150;-1928.477,1702.426;Float;False;Property;_DBHorizontalFrequency;DB Horizontal Frequency;27;0;Create;True;0;0;False;0;False;1.16;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;148;-1928.477,1894.426;Float;False;Property;_DBHorizontalMaxRadius;DB Horizontal Max Radius;29;0;Create;True;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;154;-1927.477,1799.426;Float;False;Property;_DBHorizontalPhase;DB Horizontal Phase;28;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;153;-1894.185,2803.515;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;151;-1928.477,1095.426;Float;False;Property;_DBVerticalAmplitude;DB Vertical Amplitude;20;0;Create;True;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;147;-1928.477,1191.425;Float;False;Property;_DBVerticalAmplitudeOffset;DB Vertical Amplitude Offset;21;0;Create;True;0;0;False;0;False;1.2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;152;-1928.477,1288.426;Float;False;Property;_DBVerticalFrequency;DB Vertical Frequency;22;0;Create;True;0;0;False;0;False;1.15;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;170;-1559.79,2575.282;Float;False;DB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;166;-1592.477,1287.426;Float;False;DB_VerticalFrequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;168;-1627.477,1190.425;Inherit;False;DB_VerticalAmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;159;-1560.477,1383.426;Inherit;False;DB_VerticalPhase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;164;-2554.869,1.5905;Inherit;False;1273.985;763.6335;Comment;8;216;206;205;194;193;192;190;181;World Position Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;172;-1576.477,1799.426;Inherit;False;DB_HorizontalPhase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;163;-1563.014,2488.794;Float;False;DB_PivotPosOnYAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;165;-1738.55,2797.129;Float;False;DetailBendingMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;162;-1608.477,1703.426;Float;False;DB_HorizontalFrequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;169;-1021.042,898.1936;Inherit;False;2306.147;1276.802;;27;243;237;226;225;218;211;210;207;202;201;191;189;188;187;186;185;184;183;182;180;179;178;177;176;175;174;173;Detail Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;161;-1608.477,1607.426;Float;False;DB_HorizontalAmplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;171;-1560.618,2668.292;Float;False;DB_PhaseShift;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;160;-1592.477,1095.426;Float;False;DB_VerticalAmplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;158;-1596.477,1894.426;Inherit;False;DB_HorizontalMaxRadius;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;167;-1590.477,1481.426;Inherit;False;DB_VerticalMaxLength;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;181;-2482.857,49.5325;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;185;-937.009,1511.434;Inherit;False;163;DB_PivotPosOnYAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;186;-929.1663,2092.642;Inherit;False;165;DetailBendingMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;177;-922.7522,1189.192;Inherit;False;159;DB_VerticalPhase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;184;-958.6384,2015.174;Inherit;False;158;DB_HorizontalMaxRadius;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;188;-963.3533,1688.539;Inherit;False;161;DB_HorizontalAmplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;187;-948.7522,945.1936;Inherit;False;160;DB_VerticalAmplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;183;-984.7522,1026.192;Inherit;False;168;DB_VerticalAmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;175;-924.1541,1596.542;Inherit;False;165;DetailBendingMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;182;-936.8113,1855.583;Inherit;False;172;DB_HorizontalPhase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;179;-894.2351,1269.809;Inherit;False;171;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;176;-950.7522,1107.192;Inherit;False;166;DB_VerticalFrequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;178;-965.3533,1771.539;Inherit;False;162;DB_HorizontalFrequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;174;-945.7791,1346.565;Inherit;False;167;DB_VerticalMaxLength;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;173;-912.7791,1428.565;Inherit;False;170;DB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;180;-898.8113,1936.583;Inherit;False;171;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;189;-515.0271,1177.896;Inherit;False;VerticalBending - NHP;-1;;95;41809ea7184502144ad776d88ecd1913;0;9;52;FLOAT;1;False;51;FLOAT;1;False;42;FLOAT;1;False;43;FLOAT;1;False;44;FLOAT;0;False;54;FLOAT;2;False;55;FLOAT3;0,0,0;False;53;FLOAT3;0,0,0;False;58;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TexturePropertyNode;193;-2373.11,233.2115;Inherit;True;Property;_NoiseTexture;Noise Texture;6;1;[NoScaleOffset];Create;True;0;0;False;0;False;512fa11ad89d84543ad8d6c8d9cb6743;512fa11ad89d84543ad8d6c8d9cb6743;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.FunctionNode;191;-515.5781,1819.77;Inherit;False;HorizontalBending - NHP;-1;;94;0b16e2546645f904a949bfd32be36037;0;7;44;FLOAT;1;False;39;FLOAT;1;False;43;FLOAT;1;False;40;FLOAT;0;False;46;FLOAT;2;False;47;FLOAT3;0,0,0;False;45;FLOAT;1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector4Node;190;-2425.289,438.6035;Inherit;False;Property;_NoiseTextureTilling;Noise Tilling - Static (XY), Animated (ZW);7;0;Create;False;0;0;False;0;False;1,1,1,1;0,0,0,0;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;272;-2765.307,1752.24;Inherit;False;Global;MBGlobalWindDir;MB Global Wind Dir;28;1;[HideInInspector];Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;271;-2760.943,1666.695;Float;False;Property;_MBWindDir;MB Wind Dir;16;0;Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;270;-2767.307,1832.24;Inherit;False;Property;_MBWindDirBlend;MB Wind Dir Blend;15;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector2Node;194;-2346.414,622.0544;Float;False;Property;_NoisePannerSpeed;Noise Panner Speed;8;0;Create;True;0;0;False;0;False;0.1,0.1;0.08,0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.DynamicAppendNode;192;-2276.524,76.8685;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;196;-2758.943,1564.695;Float;False;Property;_MBPhase;MB Phase;14;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;197;-2766.943,2034.695;Inherit;False;Property;_MBMaxHeight;MB Max Height;18;0;Create;True;0;0;False;0;False;10;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;198;-2765.805,1475.121;Inherit;False;Property;_MBFrequencyOffset;MB Frequency Offset;13;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;200;-2765.943,1378.695;Float;False;Property;_MBFrequency;MB Frequency;12;0;Create;True;0;0;False;0;False;1.11;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;199;-2766.232,1929.885;Float;False;Property;_MBWindDirOffset;MB Wind Dir Offset;17;0;Create;True;0;0;False;0;False;20;0;0;180;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;195;-2772.943,1283.695;Float;False;Property;_MBAmplitudeOffset;MB Amplitude Offset;11;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;202;-181.4531,1815.11;Float;False;DB_HorizontalBending;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;273;-2435.161,1730.303;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;205;-2048.317,322.0015;Inherit;False;WorldSpaceNoise - NHP;-1;;96;af5fa9ff24e18344ebcc05b64d296c57;0;4;22;FLOAT2;0,0;False;20;SAMPLER2D;;False;24;FLOAT4;1,1,1,1;False;19;FLOAT2;0.1,0.1;False;2;COLOR;0;COLOR;16
Node;AmplifyShaderEditor.RegisterLocalVarNode;201;-178.6321,1171.049;Float;False;DB_VerticalBending;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;203;-2765.943,1096.695;Float;False;Property;_MBDefaultBending;MB Default Bending;9;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;204;-2765.943,1190.695;Float;False;Property;_MBAmplitude;MB Amplitude;10;0;Create;True;0;0;False;0;False;1.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;207;205.9148,1432.427;Inherit;False;201;DB_VerticalBending;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;210;195.7927,1692.642;Inherit;False;202;DB_HorizontalBending;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;221;-2235.943,2032.695;Inherit;False;MB_MaxHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;206;-1515.889,284.0485;Inherit;False;StaticNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.Vector3Node;218;272.3818,1275.615;Float;False;Constant;_Vector0;Vector 0;27;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;208;-2252.943,1187.695;Float;False;MB_Amplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;211;275.5757,1534.743;Float;False;Constant;_Vector2;Vector 2;27;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.CommentaryNode;212;1538.883,896.0154;Inherit;False;2043.982;1275.017;;20;263;258;250;255;254;247;239;244;240;234;222;223;230;228;232;233;235;227;229;224;Main Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.CommentaryNode;231;1796.844,-256.7072;Inherit;False;1787.532;894.0291;;13;0;264;262;256;257;260;249;251;252;248;242;246;236;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;213;-2288.907,1472.279;Inherit;False;MB_FrequencyOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;216;-1513.278,384.2395;Inherit;False;AnimatedNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;215;-2300.943,1930.695;Inherit;False;MB_WindDirectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;220;-2247.943,1565.695;Inherit;False;MB_Phase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;219;-2252.943,1379.695;Float;False;MB_Frequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;214;-2284.943,1283.695;Inherit;False;MB_AmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;209;-2262.943,1724.144;Float;False;MB_WindDirection;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;217;-2284.943,1091.695;Float;False;MB_DefaultBending;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;222;1620.429,1752.861;Inherit;False;213;MB_FrequencyOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;228;1591.798,1118.268;Inherit;False;215;MB_WindDirectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;230;1657.065,1660.982;Inherit;False;219;MB_Frequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;235;1626.798,1019.268;Inherit;False;209;MB_WindDirection;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;232;1657.065,1468.982;Inherit;False;208;MB_Amplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;233;1683.083,2044.646;Inherit;False;206;StaticNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;227;1622.065,1564.982;Inherit;False;214;MB_AmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;223;1652.154,1944.818;Inherit;False;221;MB_MaxHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;234;1685.083,1853.646;Inherit;False;220;MB_Phase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;224;1643.798,1210.268;Inherit;False;216;AnimatedNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.TexturePropertyNode;236;1847.953,406.6788;Float;True;Property;_PatternTex;Pattern Texture;5;1;[NoScaleOffset];Create;False;0;0;False;0;False;50e8be33b1640034bbc8e6dacf43c1ee;50e8be33b1640034bbc8e6dacf43c1ee;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TexCoordVertexDataNode;241;-2125.385,2977.765;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;238;-2073.54,3104.806;Inherit;False;Constant;_Float2;Float 2;26;0;Create;True;0;0;False;0;False;-1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;225;504.2549,1343.611;Float;False;Property;_EnableVerticalBending;Enable Vertical Bending;19;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;229;1622.065,1371.982;Inherit;False;217;MB_DefaultBending;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;226;497.9639,1609.726;Float;False;Property;_EnableHorizontalBending;Enable Horizontal Bending;25;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TexturePropertyNode;246;2077.838,-193.9972;Float;True;Property;_MainTex;Main Texture;4;1;[NoScaleOffset];Create;False;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.TextureCoordinatesNode;242;2118.099,492.5468;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;240;1978.798,1101.268;Inherit;False;RotationAxis - NHP;-1;;99;b90648f17dcc4bc449d46e8cf04564ff;0;3;20;FLOAT;0;False;19;FLOAT;0;False;18;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;239;1985.577,1603.847;Inherit;False;RotationAngle - NHP;-1;;98;87b0b7c0fc8f1424db43b84d20c2e79b;0;9;36;FLOAT;0;False;35;FLOAT;0;False;34;FLOAT;1;False;28;FLOAT;1;False;47;FLOAT;0;False;29;FLOAT;1;False;46;FLOAT;0;False;42;FLOAT;0;False;27;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;245;-1883.822,3056.971;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;237;863.6108,1465.648;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;247;2356.624,1599.865;Float;False;MB_RotationAngle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;244;2341.797,1096.268;Inherit;False;MB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;251;2491.259,228.2338;Inherit;False;Property;_PatternColor1;Pattern Color 1;2;0;Create;True;0;0;False;0;False;1,1,1,1;1,1,1,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;243;1028.759,1461.662;Float;False;DB_VertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.ColorNode;248;2492.232,52.25374;Inherit;False;Property;_PatternColor2;Pattern Color 2;3;0;Create;True;0;0;False;0;False;0,0,0,1;0,0,0,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;253;-1714.189,3052.285;Float;False;TextureMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;252;2400.698,404.7978;Inherit;True;Property;_TextureSample0;Texture Sample 0;0;1;[NoScaleOffset];Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.TextureCoordinatesNode;249;2354.484,-111.6292;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;256;2754.547,488.3769;Inherit;False;253;TextureMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;250;2677.908,1444.06;Inherit;False;243;DB_VertexOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;255;2666.908,1349.06;Inherit;False;247;MB_RotationAngle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;254;2677.908,1251.06;Inherit;False;244;MB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.SamplerNode;260;2643.583,-192.8792;Inherit;True;Property;_MainTexture;Main Texture;0;1;[NoScaleOffset];Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;257;2776.228,216.5459;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;258;2947.444,1331.494;Inherit;False;MainBending - NHP;-1;;100;01dba1f3bc33e4b4fa301d2180819576;0;4;55;FLOAT3;0,0,0;False;53;FLOAT;0;False;59;FLOAT3;0,0,0;False;58;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.LerpOp;262;3082.011,187.2758;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;264;3267.463,184.5457;Float;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;265;3969.766,638.3008;Inherit;False;631.4954;512.0168;;5;269;268;267;266;1;Master Node;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;263;3339.296,1327.908;Float;False;LocalVertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;267;4018.589,917.0527;Inherit;False;Property;_Smoothness;Smoothness;1;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;268;4018.589,830.0527;Inherit;False;Property;_Metallic;Metallic;0;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;266;4107.276,734.2667;Inherit;False;264;Albedo;1;0;OBJECT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;269;4046.637,1009.999;Inherit;False;263;LocalVertexOffset;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;4326.652,771.3898;Float;False;True;-1;2;LowPolyTreeWithPatterns_MaterialInspector;0;2;Nicrom/NHP/ASE/Low Poly Tree With Patterns;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;17;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;;0;0;Standard;36;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;1;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;0;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;2215.454,-32.3007;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;False;False;False;False;0;False;-1;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;155;17;144;0
WireConnection;153;0;143;0
WireConnection;153;1;146;1
WireConnection;170;0;155;0
WireConnection;166;0;152;0
WireConnection;168;0;147;0
WireConnection;159;0;157;0
WireConnection;172;0;154;0
WireConnection;163;0;155;19
WireConnection;165;0;153;0
WireConnection;162;0;150;0
WireConnection;161;0;149;0
WireConnection;171;0;155;18
WireConnection;160;0;151;0
WireConnection;158;0;148;0
WireConnection;167;0;156;0
WireConnection;189;52;187;0
WireConnection;189;51;183;0
WireConnection;189;42;176;0
WireConnection;189;43;177;0
WireConnection;189;44;179;0
WireConnection;189;54;174;0
WireConnection;189;55;173;0
WireConnection;189;53;185;0
WireConnection;189;58;175;0
WireConnection;191;44;188;0
WireConnection;191;39;178;0
WireConnection;191;43;182;0
WireConnection;191;40;180;0
WireConnection;191;46;184;0
WireConnection;191;45;186;0
WireConnection;192;0;181;1
WireConnection;192;1;181;3
WireConnection;202;0;191;0
WireConnection;273;0;271;0
WireConnection;273;1;272;0
WireConnection;273;2;270;0
WireConnection;205;22;192;0
WireConnection;205;20;193;0
WireConnection;205;24;190;0
WireConnection;205;19;194;0
WireConnection;201;0;189;0
WireConnection;221;0;197;0
WireConnection;206;0;205;0
WireConnection;208;0;204;0
WireConnection;213;0;198;0
WireConnection;216;0;205;16
WireConnection;215;0;199;0
WireConnection;220;0;196;0
WireConnection;219;0;200;0
WireConnection;214;0;195;0
WireConnection;209;0;273;0
WireConnection;217;0;203;0
WireConnection;225;1;218;0
WireConnection;225;0;207;0
WireConnection;226;1;211;0
WireConnection;226;0;210;0
WireConnection;242;2;236;0
WireConnection;240;20;235;0
WireConnection;240;19;228;0
WireConnection;240;18;224;0
WireConnection;239;36;229;0
WireConnection;239;35;232;0
WireConnection;239;34;227;0
WireConnection;239;28;230;0
WireConnection;239;47;222;0
WireConnection;239;29;234;0
WireConnection;239;42;223;0
WireConnection;239;27;233;0
WireConnection;245;0;241;2
WireConnection;245;1;238;0
WireConnection;237;0;225;0
WireConnection;237;1;226;0
WireConnection;247;0;239;0
WireConnection;244;0;240;0
WireConnection;243;0;237;0
WireConnection;253;0;245;0
WireConnection;252;0;236;0
WireConnection;252;1;242;0
WireConnection;249;2;246;0
WireConnection;260;0;246;0
WireConnection;260;1;249;0
WireConnection;257;0;248;0
WireConnection;257;1;251;0
WireConnection;257;2;252;0
WireConnection;258;55;254;0
WireConnection;258;53;255;0
WireConnection;258;58;250;0
WireConnection;262;0;260;0
WireConnection;262;1;257;0
WireConnection;262;2;256;0
WireConnection;264;0;262;0
WireConnection;263;0;258;0
WireConnection;1;0;266;0
WireConnection;1;3;268;0
WireConnection;1;4;267;0
WireConnection;1;8;269;0
ASEEND*/
//CHKSM=F7B7668DA9DC83CC9A5FAB0A4D624027EC0CDECC