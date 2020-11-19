// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Nicrom/NHP/ASE/Low Poly Vegetation"
{
	Properties
	{
		[HideInInspector] _EmissionColor("Emission Color", Color) = (1,1,1,1)
		[HideInInspector] _AlphaCutoff("Alpha Cutoff ", Range(0, 1)) = 0.5
		[ASEBegin][NoScaleOffset]_MainTex("Main Texture", 2D) = "white" {}
		_Metallic("Metallic", Range( 0 , 1)) = 0
		_Smoothness("Smoothness", Range( 0 , 1)) = 0
		_MBDefaultBending("MB Default Bending", Float) = 0
		_MBAmplitude("MB Amplitude", Float) = 1.5
		_MBAmplitudeOffset("MB Amplitude Offset", Float) = 2
		_MBFrequency("MB Frequency", Float) = 1.11
		_MBFrequencyOffset("MB Frequency Offset", Float) = 0
		_MBPhase("MB Phase", Float) = 1
		_MBWindDir("MB Wind Dir", Range( 0 , 360)) = 0
		_MBWindDirOffset("MB Wind Dir Offset", Range( 0 , 180)) = 20
		_MBWindBlend("MB Wind Blend", Range( 0 , 1)) = 0
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
		[NoScaleOffset]_NoiseTexture("Noise Texture", 2D) = "white" {}
		_NoiseTextureTilling("Noise Tilling - Static (XY), Animated (ZW)", Vector) = (1,1,1,1)
		_NoisePannerSpeed("Noise Panner Speed", Vector) = (0.05,0.03,0,0)
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
			#pragma multi_compile _ LOD_FADE_CROSSFADE
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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalFrequency;
			float _DBVerticalAmplitudeOffset;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindBlend;
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

				float lerpResult205 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindBlend);
				float MB_WindDirection223 = lerpResult205;
				float MB_WindDirectionOffset226 = _MBWindDirOffset;
				float4 transform185 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult198 = (float2(transform185.x , transform185.z));
				float2 UVs27_g112 = appendResult198;
				float4 temp_output_24_0_g112 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g112 = (temp_output_24_0_g112).zw;
				float2 panner7_g112 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldNoise214 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g112 * AnimatedNoiseTilling29_g112 ) + panner7_g112 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection223 + ( MB_WindDirectionOffset226 * (-1.0 + ((AnimatedWorldNoise214).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g118;
				float3 RotationAxis56_g119 = MB_RotationAxis246;
				float MB_Amplitude222 = _MBAmplitude;
				float MB_AmplitudeOffset220 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g112 = (temp_output_24_0_g112).xy;
				float4 StaticWorldNoise221 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g112 * StaticNoileTilling28_g112 ), 0, 0.0) );
				float4 StaticWorldNoise31_g117 = StaticWorldNoise221;
				float4 transform8_g117 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency217 = _MBFrequency;
				float MB_FrequencyOffset215 = _MBFrequencyOffset;
				float MB_Phase227 = _MBPhase;
				float MB_DefaultBending213 = _MBDefaultBending;
				float MB_MaxHeight216 = _MBMaxHeight;
				float MB_RotationAngle244 = radians( ( ( ( ( MB_Amplitude222 + ( MB_AmplitudeOffset220 * (StaticWorldNoise31_g117).x ) ) * sin( ( ( ( transform8_g117.x + transform8_g117.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency217 + ( MB_FrequencyOffset215 * (StaticWorldNoise31_g117).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase227 ) ) ) + MB_DefaultBending213 ) * ( v.vertex.xyz.y / MB_MaxHeight216 ) ) );
				float RotationAngle54_g119 = MB_RotationAngle244;
				float3 PivotPoint60_g119 = float3( 0,0,0 );
				float3 break62_g119 = PivotPoint60_g119;
				float3 appendResult45_g119 = (float3(break62_g119.x , v.vertex.xyz.y , break62_g119.z));
				float3 rotatedValue30_g119 = RotateAroundAxis( appendResult45_g119, v.vertex.xyz, RotationAxis56_g119, RotationAngle54_g119 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis173 = appendResult10_g90;
				float DB_VerticalAmplitude166 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset169 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift170 = v.ase_color.a;
				float PhaseShift48_g94 = DB_PhaseShift170;
				float DB_VerticalFrequency168 = _DBVerticalFrequency;
				float Fequency45_g94 = DB_VerticalFrequency168;
				float4 transform2_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase162 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g94 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength165 = _DBVerticalMaxLength;
				float3 rotatedValue29_g94 = RotateAroundAxis( PivotPosOnYAxis56_g94, v.vertex.xyz, DB_RotationAxis173, radians( ( ( ( DB_VerticalAmplitude166 + ( DB_VerticalAmplitudeOffset169 * ( 1.0 - PhaseShift48_g94 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g94 ) - ( ( 2.0 * PI ) * PhaseShift48_g94 ) ) + ( ( ( transform2_g94.x + transform2_g94.z ) + ( ( _TimeParameters.x ) * Fequency45_g94 ) ) * DB_VerticalPhase162 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g94 ) / DB_VerticalMaxLength165 ) ) ) );
				float VerticalBendingMask172 = step( 1.0 , v.texcoord.y );
				float3 DB_VerticalMovement202 = ( ( rotatedValue29_g94 - v.vertex.xyz ) * VerticalBendingMask172 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch235 = DB_VerticalMovement202;
				#else
				float3 staticSwitch235 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude174 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency171 = _DBHorizontalFrequency;
				float Frequency41_g93 = DB_HorizontalFrequency171;
				float4 transform5_g93 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase167 = _DBHorizontalPhase;
				float3 PivotPoint49_g93 = float3( 0,0,0 );
				float3 break52_g93 = PivotPoint49_g93;
				float3 appendResult20_g93 = (float3(break52_g93.x , v.vertex.xyz.y , break52_g93.z));
				float DB_HorizontalMaxRadius175 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g93 = RotateAroundAxis( PivotPoint49_g93, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude174 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g93 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift170 ) ) ) + ( ( ( transform5_g93.x + transform5_g93.z ) + ( ( _TimeParameters.x ) * Frequency41_g93 ) ) * DB_HorizontalPhase167 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g93 ) / DB_HorizontalMaxRadius175 ) ) ) );
				float HorizontalBendingMask164 = step( 1.0 , v.texcoord.x );
				float3 DB_SideToSideMovement201 = ( ( rotatedValue33_g93 - v.vertex.xyz ) * HorizontalBendingMask164 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch237 = DB_SideToSideMovement201;
				#else
				float3 staticSwitch237 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset245 = ( staticSwitch235 + staticSwitch237 );
				float3 rotatedValue34_g119 = RotateAroundAxis( PivotPoint60_g119, ( rotatedValue30_g119 + DB_VertexOffset245 ), RotationAxis56_g119, RotationAngle54_g119 );
				float3 LocalVertexOffset257 = ( ( rotatedValue34_g119 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				o.ase_texcoord7.xy = v.texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord7.zw = 0;
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset257;
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
				float4 Albedo256 = tex2D( _MainTex, uv_MainTex );
				
				float3 Albedo = Albedo256.rgb;
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
			#pragma multi_compile _ LOD_FADE_CROSSFADE
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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalFrequency;
			float _DBVerticalAmplitudeOffset;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindBlend;
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

				float lerpResult205 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindBlend);
				float MB_WindDirection223 = lerpResult205;
				float MB_WindDirectionOffset226 = _MBWindDirOffset;
				float4 transform185 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult198 = (float2(transform185.x , transform185.z));
				float2 UVs27_g112 = appendResult198;
				float4 temp_output_24_0_g112 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g112 = (temp_output_24_0_g112).zw;
				float2 panner7_g112 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldNoise214 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g112 * AnimatedNoiseTilling29_g112 ) + panner7_g112 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection223 + ( MB_WindDirectionOffset226 * (-1.0 + ((AnimatedWorldNoise214).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g118;
				float3 RotationAxis56_g119 = MB_RotationAxis246;
				float MB_Amplitude222 = _MBAmplitude;
				float MB_AmplitudeOffset220 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g112 = (temp_output_24_0_g112).xy;
				float4 StaticWorldNoise221 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g112 * StaticNoileTilling28_g112 ), 0, 0.0) );
				float4 StaticWorldNoise31_g117 = StaticWorldNoise221;
				float4 transform8_g117 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency217 = _MBFrequency;
				float MB_FrequencyOffset215 = _MBFrequencyOffset;
				float MB_Phase227 = _MBPhase;
				float MB_DefaultBending213 = _MBDefaultBending;
				float MB_MaxHeight216 = _MBMaxHeight;
				float MB_RotationAngle244 = radians( ( ( ( ( MB_Amplitude222 + ( MB_AmplitudeOffset220 * (StaticWorldNoise31_g117).x ) ) * sin( ( ( ( transform8_g117.x + transform8_g117.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency217 + ( MB_FrequencyOffset215 * (StaticWorldNoise31_g117).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase227 ) ) ) + MB_DefaultBending213 ) * ( v.vertex.xyz.y / MB_MaxHeight216 ) ) );
				float RotationAngle54_g119 = MB_RotationAngle244;
				float3 PivotPoint60_g119 = float3( 0,0,0 );
				float3 break62_g119 = PivotPoint60_g119;
				float3 appendResult45_g119 = (float3(break62_g119.x , v.vertex.xyz.y , break62_g119.z));
				float3 rotatedValue30_g119 = RotateAroundAxis( appendResult45_g119, v.vertex.xyz, RotationAxis56_g119, RotationAngle54_g119 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis173 = appendResult10_g90;
				float DB_VerticalAmplitude166 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset169 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift170 = v.ase_color.a;
				float PhaseShift48_g94 = DB_PhaseShift170;
				float DB_VerticalFrequency168 = _DBVerticalFrequency;
				float Fequency45_g94 = DB_VerticalFrequency168;
				float4 transform2_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase162 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g94 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength165 = _DBVerticalMaxLength;
				float3 rotatedValue29_g94 = RotateAroundAxis( PivotPosOnYAxis56_g94, v.vertex.xyz, DB_RotationAxis173, radians( ( ( ( DB_VerticalAmplitude166 + ( DB_VerticalAmplitudeOffset169 * ( 1.0 - PhaseShift48_g94 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g94 ) - ( ( 2.0 * PI ) * PhaseShift48_g94 ) ) + ( ( ( transform2_g94.x + transform2_g94.z ) + ( ( _TimeParameters.x ) * Fequency45_g94 ) ) * DB_VerticalPhase162 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g94 ) / DB_VerticalMaxLength165 ) ) ) );
				float VerticalBendingMask172 = step( 1.0 , v.ase_texcoord.y );
				float3 DB_VerticalMovement202 = ( ( rotatedValue29_g94 - v.vertex.xyz ) * VerticalBendingMask172 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch235 = DB_VerticalMovement202;
				#else
				float3 staticSwitch235 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude174 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency171 = _DBHorizontalFrequency;
				float Frequency41_g93 = DB_HorizontalFrequency171;
				float4 transform5_g93 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase167 = _DBHorizontalPhase;
				float3 PivotPoint49_g93 = float3( 0,0,0 );
				float3 break52_g93 = PivotPoint49_g93;
				float3 appendResult20_g93 = (float3(break52_g93.x , v.vertex.xyz.y , break52_g93.z));
				float DB_HorizontalMaxRadius175 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g93 = RotateAroundAxis( PivotPoint49_g93, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude174 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g93 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift170 ) ) ) + ( ( ( transform5_g93.x + transform5_g93.z ) + ( ( _TimeParameters.x ) * Frequency41_g93 ) ) * DB_HorizontalPhase167 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g93 ) / DB_HorizontalMaxRadius175 ) ) ) );
				float HorizontalBendingMask164 = step( 1.0 , v.ase_texcoord.x );
				float3 DB_SideToSideMovement201 = ( ( rotatedValue33_g93 - v.vertex.xyz ) * HorizontalBendingMask164 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch237 = DB_SideToSideMovement201;
				#else
				float3 staticSwitch237 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset245 = ( staticSwitch235 + staticSwitch237 );
				float3 rotatedValue34_g119 = RotateAroundAxis( PivotPoint60_g119, ( rotatedValue30_g119 + DB_VertexOffset245 ), RotationAxis56_g119, RotationAngle54_g119 );
				float3 LocalVertexOffset257 = ( ( rotatedValue34_g119 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset257;
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
			#pragma multi_compile _ LOD_FADE_CROSSFADE
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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalFrequency;
			float _DBVerticalAmplitudeOffset;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindBlend;
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

				float lerpResult205 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindBlend);
				float MB_WindDirection223 = lerpResult205;
				float MB_WindDirectionOffset226 = _MBWindDirOffset;
				float4 transform185 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult198 = (float2(transform185.x , transform185.z));
				float2 UVs27_g112 = appendResult198;
				float4 temp_output_24_0_g112 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g112 = (temp_output_24_0_g112).zw;
				float2 panner7_g112 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldNoise214 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g112 * AnimatedNoiseTilling29_g112 ) + panner7_g112 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection223 + ( MB_WindDirectionOffset226 * (-1.0 + ((AnimatedWorldNoise214).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g118;
				float3 RotationAxis56_g119 = MB_RotationAxis246;
				float MB_Amplitude222 = _MBAmplitude;
				float MB_AmplitudeOffset220 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g112 = (temp_output_24_0_g112).xy;
				float4 StaticWorldNoise221 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g112 * StaticNoileTilling28_g112 ), 0, 0.0) );
				float4 StaticWorldNoise31_g117 = StaticWorldNoise221;
				float4 transform8_g117 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency217 = _MBFrequency;
				float MB_FrequencyOffset215 = _MBFrequencyOffset;
				float MB_Phase227 = _MBPhase;
				float MB_DefaultBending213 = _MBDefaultBending;
				float MB_MaxHeight216 = _MBMaxHeight;
				float MB_RotationAngle244 = radians( ( ( ( ( MB_Amplitude222 + ( MB_AmplitudeOffset220 * (StaticWorldNoise31_g117).x ) ) * sin( ( ( ( transform8_g117.x + transform8_g117.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency217 + ( MB_FrequencyOffset215 * (StaticWorldNoise31_g117).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase227 ) ) ) + MB_DefaultBending213 ) * ( v.vertex.xyz.y / MB_MaxHeight216 ) ) );
				float RotationAngle54_g119 = MB_RotationAngle244;
				float3 PivotPoint60_g119 = float3( 0,0,0 );
				float3 break62_g119 = PivotPoint60_g119;
				float3 appendResult45_g119 = (float3(break62_g119.x , v.vertex.xyz.y , break62_g119.z));
				float3 rotatedValue30_g119 = RotateAroundAxis( appendResult45_g119, v.vertex.xyz, RotationAxis56_g119, RotationAngle54_g119 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis173 = appendResult10_g90;
				float DB_VerticalAmplitude166 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset169 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift170 = v.ase_color.a;
				float PhaseShift48_g94 = DB_PhaseShift170;
				float DB_VerticalFrequency168 = _DBVerticalFrequency;
				float Fequency45_g94 = DB_VerticalFrequency168;
				float4 transform2_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase162 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g94 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength165 = _DBVerticalMaxLength;
				float3 rotatedValue29_g94 = RotateAroundAxis( PivotPosOnYAxis56_g94, v.vertex.xyz, DB_RotationAxis173, radians( ( ( ( DB_VerticalAmplitude166 + ( DB_VerticalAmplitudeOffset169 * ( 1.0 - PhaseShift48_g94 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g94 ) - ( ( 2.0 * PI ) * PhaseShift48_g94 ) ) + ( ( ( transform2_g94.x + transform2_g94.z ) + ( ( _TimeParameters.x ) * Fequency45_g94 ) ) * DB_VerticalPhase162 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g94 ) / DB_VerticalMaxLength165 ) ) ) );
				float VerticalBendingMask172 = step( 1.0 , v.ase_texcoord.y );
				float3 DB_VerticalMovement202 = ( ( rotatedValue29_g94 - v.vertex.xyz ) * VerticalBendingMask172 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch235 = DB_VerticalMovement202;
				#else
				float3 staticSwitch235 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude174 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency171 = _DBHorizontalFrequency;
				float Frequency41_g93 = DB_HorizontalFrequency171;
				float4 transform5_g93 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase167 = _DBHorizontalPhase;
				float3 PivotPoint49_g93 = float3( 0,0,0 );
				float3 break52_g93 = PivotPoint49_g93;
				float3 appendResult20_g93 = (float3(break52_g93.x , v.vertex.xyz.y , break52_g93.z));
				float DB_HorizontalMaxRadius175 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g93 = RotateAroundAxis( PivotPoint49_g93, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude174 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g93 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift170 ) ) ) + ( ( ( transform5_g93.x + transform5_g93.z ) + ( ( _TimeParameters.x ) * Frequency41_g93 ) ) * DB_HorizontalPhase167 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g93 ) / DB_HorizontalMaxRadius175 ) ) ) );
				float HorizontalBendingMask164 = step( 1.0 , v.ase_texcoord.x );
				float3 DB_SideToSideMovement201 = ( ( rotatedValue33_g93 - v.vertex.xyz ) * HorizontalBendingMask164 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch237 = DB_SideToSideMovement201;
				#else
				float3 staticSwitch237 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset245 = ( staticSwitch235 + staticSwitch237 );
				float3 rotatedValue34_g119 = RotateAroundAxis( PivotPoint60_g119, ( rotatedValue30_g119 + DB_VertexOffset245 ), RotationAxis56_g119, RotationAngle54_g119 );
				float3 LocalVertexOffset257 = ( ( rotatedValue34_g119 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset257;
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
			#pragma multi_compile _ LOD_FADE_CROSSFADE
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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalFrequency;
			float _DBVerticalAmplitudeOffset;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindBlend;
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

				float lerpResult205 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindBlend);
				float MB_WindDirection223 = lerpResult205;
				float MB_WindDirectionOffset226 = _MBWindDirOffset;
				float4 transform185 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult198 = (float2(transform185.x , transform185.z));
				float2 UVs27_g112 = appendResult198;
				float4 temp_output_24_0_g112 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g112 = (temp_output_24_0_g112).zw;
				float2 panner7_g112 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldNoise214 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g112 * AnimatedNoiseTilling29_g112 ) + panner7_g112 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection223 + ( MB_WindDirectionOffset226 * (-1.0 + ((AnimatedWorldNoise214).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g118;
				float3 RotationAxis56_g119 = MB_RotationAxis246;
				float MB_Amplitude222 = _MBAmplitude;
				float MB_AmplitudeOffset220 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g112 = (temp_output_24_0_g112).xy;
				float4 StaticWorldNoise221 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g112 * StaticNoileTilling28_g112 ), 0, 0.0) );
				float4 StaticWorldNoise31_g117 = StaticWorldNoise221;
				float4 transform8_g117 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency217 = _MBFrequency;
				float MB_FrequencyOffset215 = _MBFrequencyOffset;
				float MB_Phase227 = _MBPhase;
				float MB_DefaultBending213 = _MBDefaultBending;
				float MB_MaxHeight216 = _MBMaxHeight;
				float MB_RotationAngle244 = radians( ( ( ( ( MB_Amplitude222 + ( MB_AmplitudeOffset220 * (StaticWorldNoise31_g117).x ) ) * sin( ( ( ( transform8_g117.x + transform8_g117.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency217 + ( MB_FrequencyOffset215 * (StaticWorldNoise31_g117).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase227 ) ) ) + MB_DefaultBending213 ) * ( v.vertex.xyz.y / MB_MaxHeight216 ) ) );
				float RotationAngle54_g119 = MB_RotationAngle244;
				float3 PivotPoint60_g119 = float3( 0,0,0 );
				float3 break62_g119 = PivotPoint60_g119;
				float3 appendResult45_g119 = (float3(break62_g119.x , v.vertex.xyz.y , break62_g119.z));
				float3 rotatedValue30_g119 = RotateAroundAxis( appendResult45_g119, v.vertex.xyz, RotationAxis56_g119, RotationAngle54_g119 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis173 = appendResult10_g90;
				float DB_VerticalAmplitude166 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset169 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift170 = v.ase_color.a;
				float PhaseShift48_g94 = DB_PhaseShift170;
				float DB_VerticalFrequency168 = _DBVerticalFrequency;
				float Fequency45_g94 = DB_VerticalFrequency168;
				float4 transform2_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase162 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g94 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength165 = _DBVerticalMaxLength;
				float3 rotatedValue29_g94 = RotateAroundAxis( PivotPosOnYAxis56_g94, v.vertex.xyz, DB_RotationAxis173, radians( ( ( ( DB_VerticalAmplitude166 + ( DB_VerticalAmplitudeOffset169 * ( 1.0 - PhaseShift48_g94 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g94 ) - ( ( 2.0 * PI ) * PhaseShift48_g94 ) ) + ( ( ( transform2_g94.x + transform2_g94.z ) + ( ( _TimeParameters.x ) * Fequency45_g94 ) ) * DB_VerticalPhase162 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g94 ) / DB_VerticalMaxLength165 ) ) ) );
				float VerticalBendingMask172 = step( 1.0 , v.ase_texcoord.y );
				float3 DB_VerticalMovement202 = ( ( rotatedValue29_g94 - v.vertex.xyz ) * VerticalBendingMask172 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch235 = DB_VerticalMovement202;
				#else
				float3 staticSwitch235 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude174 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency171 = _DBHorizontalFrequency;
				float Frequency41_g93 = DB_HorizontalFrequency171;
				float4 transform5_g93 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase167 = _DBHorizontalPhase;
				float3 PivotPoint49_g93 = float3( 0,0,0 );
				float3 break52_g93 = PivotPoint49_g93;
				float3 appendResult20_g93 = (float3(break52_g93.x , v.vertex.xyz.y , break52_g93.z));
				float DB_HorizontalMaxRadius175 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g93 = RotateAroundAxis( PivotPoint49_g93, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude174 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g93 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift170 ) ) ) + ( ( ( transform5_g93.x + transform5_g93.z ) + ( ( _TimeParameters.x ) * Frequency41_g93 ) ) * DB_HorizontalPhase167 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g93 ) / DB_HorizontalMaxRadius175 ) ) ) );
				float HorizontalBendingMask164 = step( 1.0 , v.ase_texcoord.x );
				float3 DB_SideToSideMovement201 = ( ( rotatedValue33_g93 - v.vertex.xyz ) * HorizontalBendingMask164 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch237 = DB_SideToSideMovement201;
				#else
				float3 staticSwitch237 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset245 = ( staticSwitch235 + staticSwitch237 );
				float3 rotatedValue34_g119 = RotateAroundAxis( PivotPoint60_g119, ( rotatedValue30_g119 + DB_VertexOffset245 ), RotationAxis56_g119, RotationAngle54_g119 );
				float3 LocalVertexOffset257 = ( ( rotatedValue34_g119 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset257;
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
				float4 Albedo256 = tex2D( _MainTex, uv_MainTex );
				
				
				float3 Albedo = Albedo256.rgb;
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
			#pragma multi_compile _ LOD_FADE_CROSSFADE
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
			float4 _MainTex_ST;
			float4 _NoiseTextureTilling;
			float2 _NoisePannerSpeed;
			float _MBWindDir;
			float _DBHorizontalMaxRadius;
			float _DBHorizontalPhase;
			float _DBHorizontalFrequency;
			float _DBHorizontalAmplitude;
			float _DBVerticalMaxLength;
			float _UnitScale;
			float _DBVerticalPhase;
			float _DBVerticalFrequency;
			float _DBVerticalAmplitudeOffset;
			float _DBVerticalAmplitude;
			float _MBMaxHeight;
			float _MBDefaultBending;
			float _MBPhase;
			float _MBFrequencyOffset;
			float _MBFrequency;
			float _MBAmplitudeOffset;
			float _MBAmplitude;
			float _MBWindDirOffset;
			float _MBWindBlend;
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

				float lerpResult205 = lerp( _MBWindDir , MBGlobalWindDir , _MBWindBlend);
				float MB_WindDirection223 = lerpResult205;
				float MB_WindDirectionOffset226 = _MBWindDirOffset;
				float4 transform185 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float2 appendResult198 = (float2(transform185.x , transform185.z));
				float2 UVs27_g112 = appendResult198;
				float4 temp_output_24_0_g112 = _NoiseTextureTilling;
				float2 AnimatedNoiseTilling29_g112 = (temp_output_24_0_g112).zw;
				float2 panner7_g112 = ( 0.1 * _Time.y * _NoisePannerSpeed + float2( 0,0 ));
				float4 AnimatedWorldNoise214 = tex2Dlod( _NoiseTexture, float4( ( ( UVs27_g112 * AnimatedNoiseTilling29_g112 ) + panner7_g112 ), 0, 0.0) );
				float temp_output_11_0_g118 = radians( ( ( MB_WindDirection223 + ( MB_WindDirectionOffset226 * (-1.0 + ((AnimatedWorldNoise214).x - 0.0) * (1.0 - -1.0) / (1.0 - 0.0)) ) ) * -1.0 ) );
				float3 appendResult14_g118 = (float3(cos( temp_output_11_0_g118 ) , 0.0 , sin( temp_output_11_0_g118 )));
				float4 transform15_g118 = mul(GetWorldToObjectMatrix(),float4( appendResult14_g118 , 0.0 ));
				float3 normalizeResult34_g118 = normalize( (transform15_g118).xyz );
				float3 MB_RotationAxis246 = normalizeResult34_g118;
				float3 RotationAxis56_g119 = MB_RotationAxis246;
				float MB_Amplitude222 = _MBAmplitude;
				float MB_AmplitudeOffset220 = _MBAmplitudeOffset;
				float2 StaticNoileTilling28_g112 = (temp_output_24_0_g112).xy;
				float4 StaticWorldNoise221 = tex2Dlod( _NoiseTexture, float4( ( UVs27_g112 * StaticNoileTilling28_g112 ), 0, 0.0) );
				float4 StaticWorldNoise31_g117 = StaticWorldNoise221;
				float4 transform8_g117 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float MB_Frequency217 = _MBFrequency;
				float MB_FrequencyOffset215 = _MBFrequencyOffset;
				float MB_Phase227 = _MBPhase;
				float MB_DefaultBending213 = _MBDefaultBending;
				float MB_MaxHeight216 = _MBMaxHeight;
				float MB_RotationAngle244 = radians( ( ( ( ( MB_Amplitude222 + ( MB_AmplitudeOffset220 * (StaticWorldNoise31_g117).x ) ) * sin( ( ( ( transform8_g117.x + transform8_g117.z ) + ( ( ( _TimeParameters.x ) * ( MB_Frequency217 + ( MB_FrequencyOffset215 * (StaticWorldNoise31_g117).x ) ) ) + ( ( 2.0 * PI ) * 0.0 ) ) ) * MB_Phase227 ) ) ) + MB_DefaultBending213 ) * ( v.vertex.xyz.y / MB_MaxHeight216 ) ) );
				float RotationAngle54_g119 = MB_RotationAngle244;
				float3 PivotPoint60_g119 = float3( 0,0,0 );
				float3 break62_g119 = PivotPoint60_g119;
				float3 appendResult45_g119 = (float3(break62_g119.x , v.vertex.xyz.y , break62_g119.z));
				float3 rotatedValue30_g119 = RotateAroundAxis( appendResult45_g119, v.vertex.xyz, RotationAxis56_g119, RotationAngle54_g119 );
				float temp_output_4_0_g90 = radians( ( v.ase_color.b * 360.0 ) );
				float3 appendResult10_g90 = (float3(cos( temp_output_4_0_g90 ) , 0.0 , sin( temp_output_4_0_g90 )));
				float3 DB_RotationAxis173 = appendResult10_g90;
				float DB_VerticalAmplitude166 = _DBVerticalAmplitude;
				float DB_VerticalAmplitudeOffset169 = _DBVerticalAmplitudeOffset;
				float DB_PhaseShift170 = v.ase_color.a;
				float PhaseShift48_g94 = DB_PhaseShift170;
				float DB_VerticalFrequency168 = _DBVerticalFrequency;
				float Fequency45_g94 = DB_VerticalFrequency168;
				float4 transform2_g94 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_VerticalPhase162 = _DBVerticalPhase;
				float3 appendResult12_g90 = (float3(0.0 , ( _UnitScale * v.ase_color.g ) , 0.0));
				float3 DB_PivotPosOnYAxis163 = appendResult12_g90;
				float3 PivotPosOnYAxis56_g94 = DB_PivotPosOnYAxis163;
				float DB_VerticalMaxLength165 = _DBVerticalMaxLength;
				float3 rotatedValue29_g94 = RotateAroundAxis( PivotPosOnYAxis56_g94, v.vertex.xyz, DB_RotationAxis173, radians( ( ( ( DB_VerticalAmplitude166 + ( DB_VerticalAmplitudeOffset169 * ( 1.0 - PhaseShift48_g94 ) ) ) * sin( ( ( ( ( _TimeParameters.x ) * Fequency45_g94 ) - ( ( 2.0 * PI ) * PhaseShift48_g94 ) ) + ( ( ( transform2_g94.x + transform2_g94.z ) + ( ( _TimeParameters.x ) * Fequency45_g94 ) ) * DB_VerticalPhase162 ) ) ) ) * ( distance( v.vertex.xyz , PivotPosOnYAxis56_g94 ) / DB_VerticalMaxLength165 ) ) ) );
				float VerticalBendingMask172 = step( 1.0 , v.ase_texcoord.y );
				float3 DB_VerticalMovement202 = ( ( rotatedValue29_g94 - v.vertex.xyz ) * VerticalBendingMask172 );
				#ifdef _ENABLEVERTICALBENDING_ON
				float3 staticSwitch235 = DB_VerticalMovement202;
				#else
				float3 staticSwitch235 = float3(0,0,0);
				#endif
				float DB_HorizontalAmplitude174 = _DBHorizontalAmplitude;
				float DB_HorizontalFrequency171 = _DBHorizontalFrequency;
				float Frequency41_g93 = DB_HorizontalFrequency171;
				float4 transform5_g93 = mul(GetObjectToWorldMatrix(),float4( 0,0,0,1 ));
				float DB_HorizontalPhase167 = _DBHorizontalPhase;
				float3 PivotPoint49_g93 = float3( 0,0,0 );
				float3 break52_g93 = PivotPoint49_g93;
				float3 appendResult20_g93 = (float3(break52_g93.x , v.vertex.xyz.y , break52_g93.z));
				float DB_HorizontalMaxRadius175 = _DBHorizontalMaxRadius;
				float3 rotatedValue33_g93 = RotateAroundAxis( PivotPoint49_g93, v.vertex.xyz, float3(0,1,0), radians( ( ( DB_HorizontalAmplitude174 * sin( ( ( ( ( _TimeParameters.x ) * Frequency41_g93 ) - ( ( 2.0 * PI ) * ( 1.0 - DB_PhaseShift170 ) ) ) + ( ( ( transform5_g93.x + transform5_g93.z ) + ( ( _TimeParameters.x ) * Frequency41_g93 ) ) * DB_HorizontalPhase167 ) ) ) ) * ( distance( v.vertex.xyz , appendResult20_g93 ) / DB_HorizontalMaxRadius175 ) ) ) );
				float HorizontalBendingMask164 = step( 1.0 , v.ase_texcoord.x );
				float3 DB_SideToSideMovement201 = ( ( rotatedValue33_g93 - v.vertex.xyz ) * HorizontalBendingMask164 );
				#ifdef _ENABLEHORIZONTALBENDING_ON
				float3 staticSwitch237 = DB_SideToSideMovement201;
				#else
				float3 staticSwitch237 = float3(0,0,0);
				#endif
				float3 DB_VertexOffset245 = ( staticSwitch235 + staticSwitch237 );
				float3 rotatedValue34_g119 = RotateAroundAxis( PivotPoint60_g119, ( rotatedValue30_g119 + DB_VertexOffset245 ), RotationAxis56_g119, RotationAngle54_g119 );
				float3 LocalVertexOffset257 = ( ( rotatedValue34_g119 - v.vertex.xyz ) * step( 0.01 , v.vertex.xyz.y ) );
				
				o.ase_texcoord2.xy = v.ase_texcoord.xy;
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord2.zw = 0;
				
				#ifdef ASE_ABSOLUTE_VERTEX_POS
					float3 defaultVertexValue = v.vertex.xyz;
				#else
					float3 defaultVertexValue = float3(0, 0, 0);
				#endif
				float3 vertexValue = LocalVertexOffset257;
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
				float4 Albedo256 = tex2D( _MainTex, uv_MainTex );
				
				
				float3 Albedo = Albedo256.rgb;
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
	CustomEditor "LowPolyVegetation_MaterialInspector"
	
	
}
/*ASEBEGIN
Version=18600
2467.286;649.1429;1324;646;8324.688;2103.301;6.820844;True;False
Node;AmplifyShaderEditor.CommentaryNode;138;-6140.1,128.1405;Inherit;False;885.7786;896.2201;;13;173;172;170;164;163;158;156;151;149;148;147;146;145;Vertex Colors and UVs Baked Data;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;145;-5976.505,830.8586;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;148;-6092.223,290.6696;Float;False;Property;_UnitScale;Unit Scale;27;0;Create;True;0;0;False;0;False;20;20;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;139;-6787.008,-1284.483;Inherit;False;1529.207;1150.107;;39;227;226;223;222;220;217;216;215;213;211;210;209;208;207;206;205;204;203;199;196;195;175;174;171;169;168;167;166;165;162;161;160;159;157;155;154;153;152;150;Material Properties;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexCoordVertexDataNode;149;-5973.462,599.6374;Inherit;False;0;2;0;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RangedFloatNode;147;-5927.315,516.6774;Inherit;False;Constant;_Float2;Float 1;26;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;146;-5917.359,746.8986;Inherit;False;Constant;_Float26;Float 25;26;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;159;-5924.642,-923.6974;Float;False;Property;_DBVerticalPhase;DB Vertical Phase;17;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;160;-5923.642,-410.6974;Float;False;Property;_DBHorizontalMaxRadius;DB Horizontal Max Radius;23;0;Create;True;0;0;False;0;False;2;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;153;-5924.642,-506.6974;Float;False;Property;_DBHorizontalPhase;DB Horizontal Phase;22;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;154;-5917.642,-601.6974;Float;False;Property;_DBHorizontalFrequency;DB Horizontal Frequency;21;0;Create;True;0;0;False;0;False;1.16;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;161;-5925.642,-1210.697;Float;False;Property;_DBVerticalAmplitude;DB Vertical Amplitude;14;0;Create;True;0;0;False;0;False;1;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;150;-5923.642,-695.6974;Float;False;Property;_DBHorizontalAmplitude;DB Horizontal Amplitude;20;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;155;-5922.642,-824.6974;Float;False;Property;_DBVerticalMaxLength;DB Vertical Max Length;18;0;Create;True;0;0;False;0;False;2;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;152;-5926.642,-1017.697;Float;False;Property;_DBVerticalFrequency;DB Vertical Frequency;16;0;Create;True;0;0;False;0;False;1.15;3;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;157;-5925.642,-1115.699;Float;False;Property;_DBVerticalAmplitudeOffset;DB Vertical Amplitude Offset;15;0;Create;True;0;0;False;0;False;1.2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.FunctionNode;151;-5911.718,294.9825;Inherit;False;VertexColorData - NHP;-1;;90;0242ce46c610b224e91bc03a7bf52b77;0;1;17;FLOAT;0;False;3;FLOAT3;19;FLOAT3;0;FLOAT;18
Node;AmplifyShaderEditor.StepOpNode;156;-5744.643,802.0637;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;158;-5740.599,556.8425;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;168;-5567.642,-1018.697;Float;False;DB_VerticalFrequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;140;-4991.193,-1408.822;Inherit;False;2302.06;1276.609;;27;245;241;237;235;225;224;219;218;202;201;200;194;191;190;189;188;187;186;184;183;182;181;180;179;178;177;176;Detail Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;162;-5545.642,-923.6974;Inherit;False;DB_VerticalPhase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;163;-5509.485,219.3666;Float;False;DB_PivotPosOnYAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;164;-5584.965,550.4597;Float;False;HorizontalBendingMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;165;-5572.642,-824.6974;Inherit;False;DB_VerticalMaxLength;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;166;-5564.642,-1210.697;Float;False;DB_VerticalAmplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;141;-6654.726,-2304.694;Inherit;False;1402.304;768.1017;;8;221;214;212;198;197;193;192;185;World Space Noise;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;170;-5509.836,395.1744;Float;False;DB_PhaseShift;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;169;-5603.642,-1115.699;Inherit;False;DB_VerticalAmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;171;-5591.642,-604.6974;Float;False;DB_HorizontalFrequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;172;-5583.093,795.6794;Float;False;VerticalBendingMask;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;173;-5509.008,312.1637;Float;False;DB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;174;-5585.642,-698.6974;Float;False;DB_HorizontalAmplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;175;-5592.642,-411.6974;Inherit;False;DB_HorizontalMaxRadius;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;167;-5560.642,-506.6974;Inherit;False;DB_HorizontalPhase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;176;-4911.903,-1358.822;Inherit;False;166;DB_VerticalAmplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;177;-4857.962,-378.4335;Inherit;False;170;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;178;-4912.02,-963.4506;Inherit;False;165;DB_VerticalMaxLength;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;180;-4901.16,-801.5824;Inherit;False;163;DB_PivotPosOnYAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;179;-4901.305,-707.4735;Inherit;False;172;VerticalBendingMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;181;-4954.903,-1280.824;Inherit;False;169;DB_VerticalAmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;182;-4884.903,-1118.824;Inherit;False;162;DB_VerticalPhase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;184;-4911.903,-1199.824;Inherit;False;168;DB_VerticalFrequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.ObjectToWorldTransfNode;185;-6570.675,-2250.259;Inherit;False;1;0;FLOAT4;0,0,0,1;False;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;186;-4925.79,-296.8424;Inherit;False;175;DB_HorizontalMaxRadius;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;187;-4924.504,-534.4775;Inherit;False;171;DB_HorizontalFrequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;191;-4894.962,-455.4335;Inherit;False;167;DB_HorizontalPhase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;183;-4875.93,-880.4506;Inherit;False;173;DB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;188;-4923.504,-609.4774;Inherit;False;174;DB_HorizontalAmplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;189;-4866.386,-1040.207;Inherit;False;170;DB_PhaseShift;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;190;-4914.317,-214.3744;Inherit;False;164;HorizontalBendingMask;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;199;-6741.189,-638.3265;Float;False;Property;_MBWindDir;MB Wind Dir;9;0;Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;195;-6742.007,-559.4536;Inherit;False;Global;MBGlobalWindDir;MB Global Wind Dir;28;1;[HideInInspector];Create;True;0;0;False;0;False;0;0;0;360;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;196;-6737.007,-482.4536;Inherit;False;Property;_MBWindBlend;MB Wind Blend;11;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.DynamicAppendNode;198;-6364.342,-2222.923;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.Vector2Node;192;-6439.007,-1680.453;Float;False;Property;_NoisePannerSpeed;Noise Panner Speed;26;0;Create;True;0;0;False;0;False;0.05,0.03;0.08,0.1;0;3;FLOAT2;0;FLOAT;1;FLOAT;2
Node;AmplifyShaderEditor.TexturePropertyNode;197;-6458.129,-2066.28;Inherit;True;Property;_NoiseTexture;Noise Texture;24;1;[NoScaleOffset];Create;True;0;0;False;0;False;512fa11ad89d84543ad8d6c8d9cb6743;512fa11ad89d84543ad8d6c8d9cb6743;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.FunctionNode;194;-4492.859,-476.7446;Inherit;False;HorizontalBending - NHP;-1;;93;0b16e2546645f904a949bfd32be36037;0;7;44;FLOAT;1;False;39;FLOAT;1;False;43;FLOAT;1;False;40;FLOAT;0;False;46;FLOAT;2;False;47;FLOAT3;0,0,0;False;45;FLOAT;1;False;1;FLOAT3;0
Node;AmplifyShaderEditor.Vector4Node;193;-6522.007,-1858.453;Inherit;False;Property;_NoiseTextureTilling;Noise Tilling - Static (XY), Animated (ZW);25;0;Create;False;0;0;False;0;False;1,1,1,1;1,1,1,1;0;5;FLOAT4;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;200;-4495.178,-1131.12;Inherit;False;VerticalBending - NHP;-1;;94;41809ea7184502144ad776d88ecd1913;0;9;52;FLOAT;1;False;51;FLOAT;1;False;42;FLOAT;1;False;43;FLOAT;1;False;44;FLOAT;0;False;54;FLOAT;2;False;55;FLOAT3;0,0,0;False;53;FLOAT3;0,0,0;False;58;FLOAT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;202;-4148.783,-1135.967;Float;False;DB_VerticalMovement;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;203;-6741.057,-279.8366;Inherit;False;Property;_MBMaxHeight;MB Max Height;12;0;Create;True;0;0;False;0;False;10;10;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;205;-6426.88,-575.8046;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;206;-6746.347,-379.6415;Float;False;Property;_MBWindDirOffset;MB Wind Dir Offset;10;0;Create;True;0;0;False;0;False;20;0;0;180;0;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;201;-4101.733,-480.4045;Float;False;DB_SideToSideMovement;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;212;-6121.828,-1973.176;Inherit;False;WorldSpaceNoise - NHP;-1;;112;af5fa9ff24e18344ebcc05b64d296c57;0;4;22;FLOAT2;0,0;False;20;SAMPLER2D;;False;24;FLOAT4;1,1,1,1;False;19;FLOAT2;0.1,0.1;False;2;COLOR;0;COLOR;16
Node;AmplifyShaderEditor.RangedFloatNode;208;-6742.107,-1213.429;Float;False;Property;_MBDefaultBending;MB Default Bending;3;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;211;-6741.107,-926.4296;Float;False;Property;_MBFrequency;MB Frequency;6;0;Create;True;0;0;False;0;False;1.11;2;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;204;-6741.107,-1118.43;Float;False;Property;_MBAmplitude;MB Amplitude;4;0;Create;True;0;0;False;0;False;1.5;1.5;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;207;-6737.808,-745.7284;Float;False;Property;_MBPhase;MB Phase;8;0;Create;True;0;0;False;0;False;1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;209;-6737.725,-834.8185;Inherit;False;Property;_MBFrequencyOffset;MB Frequency Offset;7;0;Create;True;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;210;-6741.107,-1023.43;Float;False;Property;_MBAmplitudeOffset;MB Amplitude Offset;5;0;Create;True;0;0;False;0;False;2;1;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.Vector3Node;218;-3724.768,-1032.401;Float;False;Constant;_Vector2;Vector 0;27;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;227;-6221.808,-747.7284;Inherit;False;MB_Phase;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;220;-6264.107,-1024.43;Inherit;False;MB_AmplitudeOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;214;-5600.007,-1913.453;Inherit;False;AnimatedWorldNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.Vector3Node;225;-3721.574,-773.2726;Float;False;Constant;_Vector3;Vector 2;27;0;Create;True;0;0;False;0;False;0,0,0;0,0,0;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GetLocalVarNode;224;-3823.235,-876.5885;Inherit;False;202;DB_VerticalMovement;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;223;-6248.189,-581.8766;Float;False;MB_WindDirection;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;221;-5600.007,-2008.453;Inherit;False;StaticWorldNoise;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;222;-6232.107,-1120.43;Float;False;MB_Amplitude;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;226;-6276.057,-381.8325;Inherit;False;MB_WindDirectionOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;216;-6230.057,-285.8324;Inherit;False;MB_MaxHeight;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;213;-6264.107,-1216.429;Float;False;MB_DefaultBending;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.CommentaryNode;142;-2430.387,-1406.161;Inherit;False;2172.49;1277.088;;20;257;253;249;247;248;244;246;242;243;229;240;231;239;233;238;228;232;230;234;236;Main Bending;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;215;-6263.832,-834.6605;Inherit;False;MB_FrequencyOffset;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;217;-6232.107,-928.4296;Float;False;MB_Frequency;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;219;-3843.357,-618.3744;Inherit;False;201;DB_SideToSideMovement;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;233;-2259.627,-470.1625;Inherit;False;227;MB_Phase;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;238;-2316.627,-752.1625;Inherit;False;220;MB_AmplitudeOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;240;-2284.557,-375.9894;Inherit;False;216;MB_MaxHeight;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;228;-2281.627,-656.1625;Inherit;False;217;MB_Frequency;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;229;-2326.899,-557.1325;Inherit;False;215;MB_FrequencyOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;237;-3499.186,-697.2904;Float;False;Property;_EnableHorizontalBending;Enable Horizontal Bending;19;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;231;-2323.971,-1182.457;Inherit;False;226;MB_WindDirectionOffset;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;230;-2286.47,-1280.624;Inherit;False;223;MB_WindDirection;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;232;-2281.627,-848.1625;Inherit;False;222;MB_Amplitude;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;234;-2303.887,-1089.058;Inherit;False;214;AnimatedWorldNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;236;-2312.627,-943.1625;Inherit;False;213;MB_DefaultBending;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StaticSwitch;235;-3472.896,-967.7926;Float;False;Property;_EnableVerticalBending;Enable Vertical Bending;13;0;Create;True;0;0;False;0;False;0;1;1;True;;Toggle;2;Key0;Key1;Create;True;True;9;1;FLOAT3;0,0,0;False;0;FLOAT3;0,0,0;False;2;FLOAT3;0,0,0;False;3;FLOAT3;0,0,0;False;4;FLOAT3;0,0,0;False;5;FLOAT3;0,0,0;False;6;FLOAT3;0,0,0;False;7;FLOAT3;0,0,0;False;8;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;239;-2302.028,-292.4626;Inherit;False;221;StaticWorldNoise;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.FunctionNode;243;-1926.475,-716.4144;Inherit;False;RotationAngle - NHP;-1;;117;87b0b7c0fc8f1424db43b84d20c2e79b;0;9;36;FLOAT;0;False;35;FLOAT;0;False;34;FLOAT;1;False;28;FLOAT;1;False;47;FLOAT;0;False;29;FLOAT;1;False;46;FLOAT;0;False;42;FLOAT;0.1;False;27;FLOAT4;0,0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleAddOpNode;241;-3106.54,-841.3676;Inherit;False;2;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.FunctionNode;242;-1915.658,-1211.141;Inherit;False;RotationAxis - NHP;-1;;118;b90648f17dcc4bc449d46e8cf04564ff;0;3;20;FLOAT;0;False;19;FLOAT;0;False;18;FLOAT4;0,0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.CommentaryNode;143;-1531.806,-2047.234;Inherit;False;1281.093;382.0935;;4;256;255;252;250;Albedo;1,1,1,1;0;0
Node;AmplifyShaderEditor.TexturePropertyNode;250;-1431.837,-1954.525;Float;True;Property;_MainTex;Main Texture;0;1;[NoScaleOffset];Create;False;0;0;False;0;False;None;None;False;white;Auto;Texture2D;-1;0;2;SAMPLER2D;0;SAMPLERSTATE;1
Node;AmplifyShaderEditor.RegisterLocalVarNode;245;-2941.392,-845.3544;Float;False;DB_VertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;246;-1542.473,-1215.714;Inherit;False;MB_RotationAxis;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;244;-1528.033,-720.7736;Float;False;MB_RotationAngle;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.TextureCoordinatesNode;252;-1155.19,-1872.156;Inherit;False;0;-1;2;3;2;SAMPLER2D;;False;0;FLOAT2;1,1;False;1;FLOAT2;0,0;False;5;FLOAT2;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GetLocalVarNode;247;-1188.296,-803.3174;Inherit;False;245;DB_VertexOffset;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;248;-1186.296,-994.3174;Inherit;False;246;MB_RotationAxis;1;0;OBJECT;;False;1;FLOAT3;0
Node;AmplifyShaderEditor.GetLocalVarNode;249;-1195.296,-899.3174;Inherit;False;244;MB_RotationAngle;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.SamplerNode;255;-866.0916,-1953.406;Inherit;True;Property;_MainTexture1;Main Texture;0;1;[NoScaleOffset];Create;True;0;0;False;0;False;-1;None;None;True;0;False;white;Auto;False;Object;-1;Auto;Texture2D;8;0;SAMPLER2D;;False;1;FLOAT2;0,0;False;2;FLOAT;0;False;3;FLOAT2;0,0;False;4;FLOAT2;0,0;False;5;FLOAT;1;False;6;FLOAT;0;False;7;SAMPLERSTATE;;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.FunctionNode;253;-908.1885,-918.4992;Inherit;False;MainBending - NHP;-1;;119;01dba1f3bc33e4b4fa301d2180819576;0;4;55;FLOAT3;0,0,0;False;53;FLOAT;0;False;59;FLOAT3;0,0,0;False;58;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;256;-523.0996,-1952.369;Float;False;Albedo;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.CommentaryNode;144;131.218,-1025.006;Inherit;False;634.495;508.0168;;5;261;260;259;258;1;Master Node;1,1,1,1;0;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;257;-524.9075,-923.4702;Float;False;LocalVertexOffset;-1;True;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.RangedFloatNode;260;163.5042,-757.9577;Inherit;False;Property;_Smoothness;Smoothness;2;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;259;163.5042,-845.9578;Inherit;False;Property;_Metallic;Metallic;1;0;Create;True;0;0;False;0;False;0;0;0;1;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;261;256.1537,-945.5953;Inherit;False;256;Albedo;1;0;OBJECT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;258;215.5823,-657.7471;Inherit;False;257;LocalVertexOffset;1;0;OBJECT;0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;3;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;DepthOnly;0;3;DepthOnly;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;False;False;False;False;0;False;-1;False;False;False;False;True;1;False;-1;False;False;True;1;LightMode=DepthOnly;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;1;507.0764,-889.7026;Float;False;True;-1;2;LowPolyVegetation_MaterialInspector;0;2;Nicrom/NHP/ASE/Low Poly Vegetation;94348b07e5e8bab40bd6c8a1e3df54cd;True;Forward;0;1;Forward;17;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=UniversalForward;False;0;;0;0;Standard;36;Workflow;1;Surface;0;  Refraction Model;0;  Blend;0;Two Sided;1;Fragment Normal Space,InvertActionOnDeselection;0;Transmission;0;  Transmission Shadow;0.5,False,-1;Translucency;0;  Translucency Strength;1,False,-1;  Normal Distortion;0.5,False,-1;  Scattering;2,False,-1;  Direct;0.9,False,-1;  Ambient;0.1,False,-1;  Shadow;0.5,False,-1;Cast Shadows;1;  Use Shadow Threshold;0;Receive Shadows;1;GPU Instancing;1;LOD CrossFade;1;Built-in Fog;1;_FinalColorxAlpha;0;Meta Pass;1;Override Baked GI;0;Extra Pre Pass;0;DOTS Instancing;0;Tessellation;0;  Phong;0;  Strength;0.5,False,-1;  Type;0;  Tess;16,False,-1;  Min;10,False,-1;  Max;25,False,-1;  Edge Length;16,False,-1;  Max Displacement;25,False,-1;Vertex Position,InvertActionOnDeselection;1;0;6;False;True;True;True;True;True;False;;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;5;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Universal2D;0;5;Universal2D;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;1;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;True;True;True;True;0;False;-1;False;False;False;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;LightMode=Universal2D;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;2;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ShadowCaster;0;2;ShadowCaster;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;True;0;False;-1;False;False;False;False;False;False;True;1;False;-1;True;3;False;-1;False;True;1;LightMode=ShadowCaster;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;4;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;Meta;0;4;Meta;0;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;False;False;False;False;False;False;False;False;False;True;2;False;-1;False;False;False;False;False;False;False;False;True;1;LightMode=Meta;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;0;0,0;Float;False;False;-1;2;UnityEditor.ShaderGraph.PBRMasterGUI;0;1;New Amplify Shader;94348b07e5e8bab40bd6c8a1e3df54cd;True;ExtraPrePass;0;0;ExtraPrePass;5;False;False;False;False;False;False;False;False;True;0;False;-1;True;0;False;-1;False;False;False;False;False;False;False;False;True;3;RenderPipeline=UniversalPipeline;RenderType=Opaque=RenderType;Queue=Geometry=Queue=0;True;0;0;True;1;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;False;False;False;False;False;False;False;False;True;0;False;-1;True;True;True;True;True;0;False;-1;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;0;False;0;Hidden/InternalErrorShader;0;0;Standard;0;False;0
WireConnection;151;17;148;0
WireConnection;156;0;146;0
WireConnection;156;1;145;2
WireConnection;158;0;147;0
WireConnection;158;1;149;1
WireConnection;168;0;152;0
WireConnection;162;0;159;0
WireConnection;163;0;151;19
WireConnection;164;0;158;0
WireConnection;165;0;155;0
WireConnection;166;0;161;0
WireConnection;170;0;151;18
WireConnection;169;0;157;0
WireConnection;171;0;154;0
WireConnection;172;0;156;0
WireConnection;173;0;151;0
WireConnection;174;0;150;0
WireConnection;175;0;160;0
WireConnection;167;0;153;0
WireConnection;198;0;185;1
WireConnection;198;1;185;3
WireConnection;194;44;188;0
WireConnection;194;39;187;0
WireConnection;194;43;191;0
WireConnection;194;40;177;0
WireConnection;194;46;186;0
WireConnection;194;45;190;0
WireConnection;200;52;176;0
WireConnection;200;51;181;0
WireConnection;200;42;184;0
WireConnection;200;43;182;0
WireConnection;200;44;189;0
WireConnection;200;54;178;0
WireConnection;200;55;183;0
WireConnection;200;53;180;0
WireConnection;200;58;179;0
WireConnection;202;0;200;0
WireConnection;205;0;199;0
WireConnection;205;1;195;0
WireConnection;205;2;196;0
WireConnection;201;0;194;0
WireConnection;212;22;198;0
WireConnection;212;20;197;0
WireConnection;212;24;193;0
WireConnection;212;19;192;0
WireConnection;227;0;207;0
WireConnection;220;0;210;0
WireConnection;214;0;212;16
WireConnection;223;0;205;0
WireConnection;221;0;212;0
WireConnection;222;0;204;0
WireConnection;226;0;206;0
WireConnection;216;0;203;0
WireConnection;213;0;208;0
WireConnection;215;0;209;0
WireConnection;217;0;211;0
WireConnection;237;1;225;0
WireConnection;237;0;219;0
WireConnection;235;1;218;0
WireConnection;235;0;224;0
WireConnection;243;36;236;0
WireConnection;243;35;232;0
WireConnection;243;34;238;0
WireConnection;243;28;228;0
WireConnection;243;47;229;0
WireConnection;243;29;233;0
WireConnection;243;42;240;0
WireConnection;243;27;239;0
WireConnection;241;0;235;0
WireConnection;241;1;237;0
WireConnection;242;20;230;0
WireConnection;242;19;231;0
WireConnection;242;18;234;0
WireConnection;245;0;241;0
WireConnection;246;0;242;0
WireConnection;244;0;243;0
WireConnection;252;2;250;0
WireConnection;255;0;250;0
WireConnection;255;1;252;0
WireConnection;253;55;248;0
WireConnection;253;53;249;0
WireConnection;253;58;247;0
WireConnection;256;0;255;0
WireConnection;257;0;253;0
WireConnection;1;0;261;0
WireConnection;1;3;259;0
WireConnection;1;4;260;0
WireConnection;1;8;258;0
ASEEND*/
//CHKSM=E208E68EE13FC2AD3CD6832BB2A649524DCAE181