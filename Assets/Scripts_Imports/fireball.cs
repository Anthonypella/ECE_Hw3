using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class fireball : MonoBehaviour
{
    public Transform startPosition;
    public GameObject fireBall;
    private playerController p;
    public float castTime;

    private void Start()
    {
        p = playerController.instance;
        //transform.LookAt(p.enemy.transform);
    }
    public void castFireball()
    {
        Instantiate(fireBall, startPosition.position, Quaternion.identity).transform.LookAt(p.enemy.transform);
        StartCoroutine(canCastAgain());
        
    }
    public IEnumerator canCastAgain()
    {
        yield return new WaitForSeconds(castTime);
        p.isCasting = false;
    }
}
