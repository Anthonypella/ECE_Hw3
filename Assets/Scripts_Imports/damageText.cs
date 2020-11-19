using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class damageText : MonoBehaviour
{
    public float upSpeed;
    public float lifetime = 2f;
    private Transform cam;
    // Start is called before the first frame update
    void Start()
    {
        cam = Camera.main.transform;
        transform.forward = cam.forward;
        Destroy(transform.gameObject, lifetime);
    }

    // Update is called once per frame
    void Update()
    {
        transform.forward = cam.forward;
        transform.position += upSpeed * Vector3.up * Time.deltaTime;
    }
}
