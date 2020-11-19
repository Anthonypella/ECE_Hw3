using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class projectile_Explosion : MonoBehaviour
{
    public float speed;
    public float maxDistance;
    public LayerMask enemyLayer;

    public float radius = 2;
    public int damage = 25;

    public bool slows;
    public float slowAmt = .75f;
    public float slowTime = 1.5f;

    public GameObject explosion;
    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        Vector3 travel = transform.forward * speed * Time.deltaTime;
        transform.position += travel;
        maxDistance -= travel.magnitude;
        if(maxDistance <= 0)
        {
            explode();
        }
    }
    void explode()
    {
        Instantiate(explosion, transform.position, Quaternion.identity);
        Collider[] enemyColliders = Physics.OverlapSphere(transform.position, radius,enemyLayer);
        foreach (Collider item in enemyColliders)
        {
            health h = item.GetComponent<health>();
            h.takeDamage(damage, true);
            if (slows)
            {
                h.slow(slowAmt, slowTime);
            }
        }


        Destroy(transform.gameObject);
    }
    private void OnTriggerEnter(Collider other)
    {
        explode();
    }
}
