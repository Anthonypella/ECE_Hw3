using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class spellCast : MonoBehaviour
{
    public Animator orbAnim;
    public Animator wandAnim;
    public GameObject leftAttack;
    public GameObject rightAttack;
    public Transform leftSpawnPoint;
    public Transform rightSpawnPoint;
    public float leftCooldown = .25f;
    public float rightCooldown = .25f;
    public float leftCastTime = .125f;
    public float rightCastTime = .25f;
    public int leftManaCost;
    public int righManaCost;
    bool leftCanAttack = true;
    bool rightCanAttack = true;
    private Transform cam;
    private PlayerHealth h;
    private void Start()
    {
        cam = Camera.main.transform;
        h = GetComponent<PlayerHealth>();
    }
    // Update is called once per frame
    void Update()
    {
        if (Input.GetMouseButtonDown(0) && leftCanAttack && h.spendMana(leftManaCost))
        {
            //trigger wand attack
            
            orbAnim.SetTrigger("Attack");
            StartCoroutine(startLeftCooldown());
            StartCoroutine(castLeft());


        }
        if (Input.GetMouseButtonDown(1) && rightCanAttack && h.spendMana(righManaCost))
        {
            //trigger wand attack

            wandAnim.SetTrigger("Attack");
            StartCoroutine(startRightCooldown());
            StartCoroutine(castRight());
        }



    }

    public IEnumerator startLeftCooldown()
    {
        leftCanAttack = false;
        yield return new WaitForSeconds(leftCooldown);
        leftCanAttack = true;
    }
    public IEnumerator startRightCooldown()
    {
        rightCanAttack = false;
        yield return new WaitForSeconds(rightCooldown);
        rightCanAttack = true;
    }
    public IEnumerator castRight()
    {
        
        yield return new WaitForSeconds(rightCastTime);
        Instantiate(rightAttack, rightSpawnPoint.position, Quaternion.identity).transform.LookAt(getTargetPoint());
    }
    public IEnumerator castLeft()
    {

        yield return new WaitForSeconds(leftCastTime);
        Instantiate(leftAttack, leftSpawnPoint.position, Quaternion.identity).transform.LookAt(getTargetPoint());
    }
    Vector3 getTargetPoint()
    {
        return cam.position + cam.forward * 100f;
    }



}
