using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;
public class enemy_Controller : MonoBehaviour
{
    playerAnimator anim;
    playerMotor motor;

    float timer;

    private Vector3 anchorPoint;
    private bool agroed = false;
    private bool canPatrol = true;
    private bool canAgro = true;
    private bool canAttack = true;
    private Transform player;

    public float agroRadius;
    public float attackRange = 2;
    public float checkAgroTimer = .5f;
    public float patrolRadius = 10f;
    public float patrolTime = 4.0f;
    public float patrolTimeRandom = 1.5f;
    public float attackSpeed = 2f;
    public float attackDelayT = .65f;

    public int damage;
    public float range;
    // Start is called before the first frame update
    void Start()
    {
        player = GameObject.FindWithTag("Player").transform;
        anim = GetComponent<playerAnimator>();
        motor = GetComponent<playerMotor>();
        anchorPoint = transform.position;
        anchorPoint.y = 0;
        NavMeshHit hit;
        if (NavMesh.SamplePosition(transform.position, out hit, 10f, NavMesh.AllAreas))
        {
            transform.position = hit.position;

        }
        else
        {
            Destroy(transform.gameObject);
        }
        Destroy(transform.gameObject, 45f);

    }

    // Update is called once per frame
    void Update()
    {
        if (canPatrol)
        {
            
            StartCoroutine(setPatrol(patrolTime + Random.Range(-patrolTimeRandom,patrolTimeRandom)));
        }
        if(!agroed && canAgro)
        {
            timer -= Time.deltaTime;
            if(timer <= 0)
            {
                if(Vector3.Distance(transform.position,player.position) < agroRadius)
                {
                    agroed = true;
                    motor.setTarget(player);
                    canPatrol = false;
                }
                timer = checkAgroTimer;
            }
        }
        if (agroed)
        {       
            motor.setLocation(player.position);
            if (motor.isInAttackRange() && canAttack)
            {
                Debug.Log("Attacking Player");
                StartCoroutine(attackDelay());
                anim.setTrigger("Attack");
                motor.setTarget(player);
                motor.stop();
                StartCoroutine(startToResetAttack());
                





            }
            
        }




    }
    IEnumerator startToResetAttack()
    {
        canAttack = false;
        yield return new WaitForSeconds(attackSpeed);
        canAttack = true;
    }
    IEnumerator attackDelay()
    {
        yield return new WaitForSeconds(attackDelayT);
        Attack();

    }
    public void agro()
    {
        agroed = true;
        motor.setTarget(player);
        canPatrol = false;
    }
    public void Attack()
    {
        dealDamageToPlayer();
    }
    public void resetAttack()
    {
        canAttack = true;
        
    }
    void dealDamageToPlayer()
    {
        if(Vector3.Distance(transform.position,player.position) <= range)
        {
            player.GetComponent<PlayerHealth>().takeDamage(damage, false);
        }
    }
   
    public IEnumerator setPatrol(float patrolTime)
    {
        canPatrol = false;
        Debug.Log("Patrolling");
        //motor.nullTarget();
        motor.setLoc(anchorPoint + makeDisk(patrolRadius, 3f, transform.position.y));
        yield return new WaitForSeconds(patrolTime);
        canPatrol = true; 




    }
    public void slowDown(float slowAmt, float time)
    {
        StartCoroutine(motor.slowDown(slowAmt, time));
    }


    Vector3 makeDisk(float radius,float random, float yVal)
    {
        Vector2 disk = Random.insideUnitCircle * (radius + Random.Range(-random, random));
        Vector3 finalDisk = new Vector3(disk.x, yVal, disk.y);
        return finalDisk;
    }
}
