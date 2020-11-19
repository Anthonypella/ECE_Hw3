using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;

public class playerAnimator : MonoBehaviour
{
    private Animator anim;
    private NavMeshAgent agent;
    //private bool moving = true;
    // Start is called before the first frame update
    void Start()
    {
        anim = transform.GetComponent<Animator>();
        agent = GetComponent<NavMeshAgent>();
    }

    // Update is called once per frame
    void Update()
    {
        
        anim.SetFloat("Speed", agent.velocity.magnitude / agent.speed);
    }
    public void setTrigger(string name)
    {
        anim.SetTrigger(name);
    }
}
