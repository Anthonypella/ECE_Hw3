using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class cameraFollow : MonoBehaviour
{
    public Vector3 offset;
    private Transform target;
    public float followSpeed = .125f;
    public float zoomSpeed = 1f;
    private float zoom = 7.5f;
    public float maxZoom = 15f;
    public float minZoom = 5f;
    void Start()
    {
        target = GameObject.FindWithTag("Player").transform;
        zoom = (maxZoom + minZoom) / 2f;
        transform.position = target.position + (zoom * offset);
        transform.LookAt(target);
    }

    // Update is called once per frame
    void LateUpdate()
    {
        zoom = Mathf.Clamp(-Input.GetAxis("Mouse ScrollWheel") * zoomSpeed + zoom, minZoom, maxZoom);
        Vector3 targetPos = target.position + (zoom * offset);
        Vector3 lerpPos = Vector3.Lerp(transform.position, targetPos, followSpeed);
        transform.position = lerpPos;
        
    }
}
